# Call population census block data using tidycensus package
get_pop <- function(
                geography, 
                variables,
                state,
                year, 
                api_key  = NULL, 
                geometry = TRUE
                ) {

  logger::log_info("Downloading US census population data...")
  
  if (is.null(api_key)) {
    
    # Call Census data using Tidycensus
    pop <- tidycensus::get_decennial(
                              geography = geography, 
                              variables = variables,
                              state     = state,
                              year      = year,
                              geometry  = TRUE
                              )
    
    
    # Tidy data
    tidy_pop <- 
      pop %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        county        = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 3),
        block_group   = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 1),
        census_tract  = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 2),
        state         = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 4),
        population    = value
        ) %>%
      dplyr::select(state, county, block_group, census_tract, population, geoid, geometry)
    
    return(tidy_pop)
    
  } else if(!is.null(api_key)) {
    
      logger::log_info("Inserting CENSUS API key:\n{api_key}")
    
      tidycensus::census_api_key(api_key, install = TRUE)
      
      # Call Census data using Tidycensus
      pop <- tidycensus::get_decennial(
        geography = geography, 
        variables = variables,
        state     = state,
        year      = year,
        geometry  = TRUE
      )
      
      
      # Tidy data
      tidy_pop <- 
        pop %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          county        = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 3),
          block_group   = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 1),
          census_tract  = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 2),
          state         = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 4),
          population    = value
        ) %>%
        dplyr::select(state, county, block_group, census_tract, population, geoid, geometry)
    
    return(tidy_pop)
  }

}

#list of states surrounding CO for population census block data
all_states <- c("04", "08", "20", "31", "35", "40", "49", "56")

# Snap points to nearest line segment within a max distance
st_snap_points = function(
                        x, 
                        y, 
                        max_dist = 1000
                        ) {
  
  logger::log_info("Snapping points to nearest linestrings")
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(
          c,
          lapply(seq(n), function(i) {
            nrst     = sf::st_nearest_points(sf::st_geometry(x)[i], y)
            nrst_len = sf::st_length(nrst)
            nrst_mn  = which.min(nrst_len)
            
            if (as.vector(nrst_len[nrst_mn]) > max_dist) return(sf::st_geometry(x)[i])
            
              return(sf::st_cast(nrst[nrst_mn], "POINT")[2])
            }
           )
    )
  return(out)
}

# Splits linestring by points and calculates distances between each point 
segment_distance <- function(
                          points,
                          flowlines,
                          unit = "meters"
                          ) {
  
  logger::log_info("Splitting linestrings by points")
  
  
  # points to split lines by
  pts <- 
    points %>%
    sf::st_transform(5070) %>% 
    dplyr::mutate(new_id = 1:n()) %>% 
    nhdplusTools::rename_geometry("geometry") %>% 
    dplyr::select(new_id)
  
  # network of mainstems
  flines  <-
    flowlines %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(5070) %>% 
    sf::st_combine() %>% 
    sf::st_line_merge() 
  
  # get nodes of line segments
  flines_nodes <-
    flines %>% 
    sf::st_coordinates() %>% 
    as.data.frame() %>% 
    sf::st_as_sf(
      coords = c("X", "Y"),
      crs    = 5070
    )
  
  # Identify nearest node to LHD pts
  pts$ind <- sf::st_nearest_feature(pts, flines_nodes)
  
  # make the geometry of the nearest fline_node the geometry for each LHD pt
  sf::st_geometry(pts) <- sf::st_geometry(flines_nodes$geometry[pts$ind])
  
  logger::log_info("Calculating distances between points along path")
  
  # split flowines by points
  xx <- 
    flines %>% 
    lwgeom::st_split(pts) %>% 
    sf::st_collection_extract("LINESTRING") %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(l = as.numeric(sf::st_length(.)))
  
  # end and starts of each split
  end <- start <- xx
  
  # get the end node for each split string
  sf::st_geometry(end)   <- nhdplusTools::get_node(xx, "end")$geometry
  end   <- dplyr::rename(end, us_length = l)     # end   <- rename(end, ds_length = l)
  
  
  # get the start node for each split string
  sf::st_geometry(start) <- nhdplusTools::get_node(xx, "start")$geometry
  start <-  dplyr::rename(start, ds_length = l)  # start <-  rename(start, us_length = l)
  
  # End point distances
  end_map <- sf::st_intersects(pts, end)
  emap <- data.frame(
    new_id = pts$new_id[rep(1:length(end_map), times = lengths(end_map))], 
    e  = unlist(end_map)
  ) %>% 
    dplyr::filter(new_id != e)
  
  # Start point distances
  start_map <- sf::st_intersects(pts, start)
  smap <-  data.frame(
    new_id = pts$new_id[rep(1:length(start_map), times = lengths(start_map))],
    s  = unlist(start_map)
  ) %>% 
    dplyr::filter(new_id != s)
  
  # Join start & end distances
  point_distances <- full_join(
    smap, 
    emap, 
    by = "new_id"
  )
  
  point_distances$us_length    <-  end$us_length[point_distances$e]
  point_distances$ds_length    <-  start$ds_length[point_distances$s]
  
  # Select new_id and upstream/downstream lengths
  point_distances <- 
    point_distances %>% 
    dplyr::select(new_id, us_length, ds_length) %>% 
    tidyr::replace_na(
      list(
        us_length = 0, 
        ds_length = 0
      )
    ) %>% 
    dplyr::mutate(
      total_length = us_length + ds_length
      )
  
  if (unit == "meters") {

    logger::log_info("Distances in {unit}")
    
    point_distances <- 
      point_distances %>% 
      dplyr::mutate(
        new_id     = as.character(new_id),
        unit       = unit
      ) %>% 
      tibble::tibble()
    
    return(point_distances)
    
  } else {
    
    logger::log_info("Distances converted from meters to {unit}")
    
    point_distances <- 
      point_distances %>% 
      dplyr::mutate(
        us_length    = units::set_units(us_length, "meters"),
        ds_length    = units::set_units(ds_length, "meters"),
        total_length = units::set_units(total_length, "meters")
      )
    
    units(point_distances$us_length)    <- unit
    units(point_distances$ds_length)    <- unit
    units(point_distances$total_length) <- unit
    
    point_distances <- 
      point_distances %>% 
      dplyr::mutate(
        new_id       = as.character(new_id),
        us_length    = as.numeric(us_length),
        ds_length    = as.numeric(ds_length),
        total_length = as.numeric(total_length),
        unit         = unit
      ) %>% 
      tibble::tibble()
    
    return(point_distances)
  }
  
}

# Standardization methods

# Normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

# Robust scalar normalization
robust_scalar<- function(x){
  (x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))
}

# Min-Max Normalization
norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}

# Mean Normalization
mean_norm_minmax <- function(x){
  (x- mean(x)) /(max(x)-min(x))
}

