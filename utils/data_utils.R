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
  pt <- 
    points %>%
    sf::st_transform(5070) %>% 
    dplyr::mutate(new_id = 1:n()) %>%
    nhdplusTools::rename_geometry("geometry") %>% 
    dplyr::select(new_id)
  
  # network of mainstems
  network_lines  <-
    flowlines %>%
    sf::st_as_sf() %>% 
    sf::st_transform(5070) %>% 
    sf::st_combine() %>% 
    sf::st_line_merge() 
  
  # get nodes of line segments
  network_nodes <-
    network_lines %>% 
    sf::st_coordinates() %>% 
    as.data.frame() %>% 
    sf::st_as_sf(
      coords = c("X", "Y"),
      crs    = 5070
    )
  
  # Identify nearest node to LHD pts
  pt$ind <- sf::st_nearest_feature(pt, network_nodes)
  
  # make the geometry of the nearest fline_node the geometry for each LHD pt
  sf::st_geometry(pt) <- sf::st_geometry(network_nodes$geometry[pt$ind])
  
  logger::log_info("Calculating distances between points along path")
  
  # split flowines by points
  xx <- 
    network_lines %>% 
    lwgeom::st_split(pt) %>% 
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
  end_map <- sf::st_intersects(pt, end)
  emap <- data.frame(
    new_id = pt$new_id[rep(1:length(end_map), times = lengths(end_map))], 
    e  = unlist(end_map)
  ) %>% 
    dplyr::filter(new_id != e)
  
  # Start point distances
  start_map <- sf::st_intersects(pt, start)
  smap <-  data.frame(
    new_id = pt$new_id[rep(1:length(start_map), times = lengths(start_map))],
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

# isolates river segments that are not touching/intersecting
seperate_trees <- function(lines, predicate = "intersects") {
  if (predicate == "intersects") {
    
    # Lines that intersect
    index <- sf::st_intersects(lines)
    
  } else if(predicate == "touches") {
    
    # Lines that touch
    index <- sf::st_touches(lines)
    
  } else {
    logger::log_error("Predicate argument must equal 'intersects' or 'touches'")
  }
  
  # Create graph
  line_graph    <- igraph::graph_from_adj_list(index)
  
  # Extract components of graph
  line_comps    <- igraph::components(line_graph)$membership
  
  line_sections <- 
    lines %>% 
    dplyr::group_by(section = as.character({{line_comps}})) %>% 
    dplyr::summarise()
  
  return(line_sections)
}

# Prepare flowlines for use w/ sfnetworks
prep_flines <- function(flowlines, split = TRUE) {
  logger::log_info("Cleaning network...")
  
  # trim waterbody out of network
  trim <- 
    flowlines %>% 
    sf::st_transform(5070) %>% 
    nhdplusTools::align_nhdplus_names() %>% 
    nhdplusTools::get_tocomid() %>% 
    dplyr::mutate(
      wbareacomi  = as.character(wbareacomi),
      wbareacomi  = dplyr::case_when(
        wbareatype == "StreamRiver"    ~ "0",
        TRUE                           ~ wbareacomi
      ),
      wbareacomi  = dplyr::case_when(
        wbareacomi == "-9998"          ~ "0",
        TRUE                           ~ wbareacomi
      )
    ) %>%
    dplyr::mutate(wbareacomi  = as.integer(wbareacomi))
  
  # Waterbody points
  # group by waterbody & return centroid of most downstream pt for each waterbody
  wb_pts <-
    trim %>% 
    dplyr::filter(wbareacomi != 0) %>% 
    dplyr::group_by(wbareacomi) %>%
    dplyr::arrange(hydroseq, .by_group = T) %>%
    dplyr::slice_head() %>%
    sf::st_centroid() %>% 
    dplyr::ungroup() %>% 
    # group_by(comid) %>% 
    dplyr::mutate(
      new_id       = paste0("waterbody_", row_number()),
      comid        = as.character(comid),
      hydroseq     = as.character(hydroseq),
      streamleve   = as.character(streamleve),
      streamorde   = as.character(streamorde ),
      levelpathi   = as.character(levelpathi )
    ) %>% 
    dplyr::select(
      new_id, comid, wbareacomi, hydroseq, streamleve,
      streamorde, streamcalc, levelpathi, wbareatype, lakefract, surfarea
    ) %>% 
    dplyr::ungroup()
  
  logger::log_info("Sorting network...")
  
  # create input dataframe for sorting flowlines
  flowline_sort_df <- data.frame(
    ID     = trim$comid,
    toID   = trim$tocomid,
    nameID = trim$gnis_id,
    weight = trim$arbolatesu,
    stringsAsFactors = FALSE
  )
  
  # GET levelpaths for river network
  trim_sort <- nhdplusTools::get_levelpaths(flowline_sort_df) 
  
  # Join Sorted upstream tributaries with topological sort column
  ut_sort <-
    trim %>% 
    dplyr::left_join(
      trim_sort, by = c("comid" = "ID")
    ) %>% 
    nhdplusTools::get_sorted(split = split)
  
  logger::log_info("Topological sorting")
  if (split == TRUE) {
    # Topologically sorted flowline network
    flines <-
      ut_sort %>% 
      dplyr::select(comid, tocomid, topo_sort, levelpath ,                   
                    streamleve, streamorde, streamcalc, hydroseq, 
                    terminalID, reachcode, geometry) %>%
      dplyr::arrange(topo_sort)
    
    logger::log_info("Rounding geometry precision")
    
    # round edge precision to make sure edges connect exactly
    sf::st_geometry(flines) <-
      sf::st_geometry(flines) %>%
      lapply(function(x) round(x, 0)) %>%
      sf::st_sfc(crs = sf::st_crs(flines))
    
    return(flines)
  } else if(split == FALSE) {
    
  # Topologically sorted flowline network
  flines <-
    ut_sort %>% 
    dplyr::select(comid, tocomid, topo_sort, levelpath ,                   
                  streamleve, streamorde, streamcalc,
                  hydroseq, reachcode, geometry) %>%
    dplyr::arrange(topo_sort)
  
  logger::log_info("Rounding geometry precision")
  
  # round edge precision to make sure edges connect exactly
  sf::st_geometry(flines) <-
    sf::st_geometry(flines) %>%
    lapply(function(x) round(x, 0)) %>%
    sf::st_sfc(crs = sf::st_crs(flines))
  
  return(flines)
  
}
  
}

# Prepare points for use w/ sfnetworks
prep_points <- function(flowlines, points, wb = FALSE) {
  
  if (wb == FALSE) {
    logger::log_info("Preparing LHD points as nodes")
    
    # Preparing LHD points
    
    # LHD points in trimmed network
    pts <-
      points %>% 
      dplyr::filter(COMID %in% flowlines$comid) %>% 
      # dplyr::filter(COMID %in% ut_sort$comid) %>% 
      sf::st_transform(5070)  %>% 
      dplyr::select(new_id, 
                    comid      = COMID,
                    hydroseq   = Hydroseq, 
                    streamleve, 
                    streamorde = StreamOrde,
                    levelpathi = LevelPathI,
                    reachcode  = REACHCODE
      ) 
    # Nodes to blend into graph
    fline_nodes <- 
      pts %>% 
      dplyr::select(new_id, comid,  streamleve, streamorde, hydroseq, reachcode, geometry) %>%
      dplyr::mutate(is_point = TRUE)
    
    # Snap LHD pts to flowlines on graph
    sf::st_geometry(fline_nodes) <- sf::st_geometry(
      st_snap_points(fline_nodes, flowlines)
    )
    
    return(fline_nodes)
    
  } else if(wb == TRUE) {
    
    logger::log_info("Locating waterbodies")
    
    # Waterbody points
    # group by waterbody & return centroid of most downstream pt for each waterbody
    wb_pts <-
      flowlines %>% 
      dplyr::filter(wbareacomi != 0) %>% 
      dplyr::group_by(wbareacomi) %>%
      dplyr::arrange(hydroseq, .by_group = T) %>%
      dplyr::slice_head() %>%
      sf::st_centroid() %>% 
      dplyr::ungroup() %>% 
      # group_by(comid) %>% 
      dplyr::mutate(
        new_id       = paste0("waterbody_", row_number()),
        comid        = as.character(comid),
        hydroseq     = as.character(hydroseq),
        streamleve   = as.character(streamleve),
        streamorde   = as.character(streamorde ),
        levelpathi   = as.character(levelpathi )
      ) %>% 
      dplyr::select(
        new_id, comid, wbareacomi, hydroseq, streamleve,
        streamorde, levelpathi, reachcode, wbareatype, lakefract, surfarea
      ) %>% 
      dplyr::ungroup()
    
    logger::log_info("Preparing LHD and waterbody points as nodes")
    
    # LHD points in trimmed network,  Bind LHD points with waterbody points
    pts <-
      points %>% 
      dplyr::filter(COMID %in% flowlines$comid) %>% 
      # dplyr::filter(COMID %in% ut_sort$comid) %>% 
      sf::st_transform(5070)  %>% 
      dplyr::select(new_id, 
                    comid      = COMID,
                    hydroseq   = Hydroseq, 
                    streamleve, 
                    streamorde = StreamOrde,
                    levelpathi = LevelPathI,
                    reachcode  = REACHCODE
      ) %>%
      dplyr::bind_rows(wb_pts) %>%
      dplyr::select(new_id:geometry)
    
    # Nodes to blend into graph
    fline_nodes <- 
      pts %>% 
      dplyr::select(new_id, comid,  streamleve, streamorde, hydroseq, reachcode, geometry) %>%
      dplyr::mutate(is_point = TRUE)
    
    # Snap LHD pts to flowlines on graph
    sf::st_geometry(fline_nodes) <- sf::st_geometry(
      st_snap_points(fline_nodes, flowlines)
    )
    
    return(fline_nodes)
    
  }

}

# Function uses stream network flowlines and supplied LHD points to create a simple directed graph (rooted tree)
# WB, logical that determines whether waterbody points will be used in addition to supplied points when segmenting graph.
create_network <- function(flowlines, points, wb = FALSE) {

  logger::log_info("Creating network")
  
  # Network Graph
  net <- 
    # flines %>%
    flowlines %>%
    sfnetworks::as_sfnetwork() %>% 
    tidygraph::convert(to_spatial_subdivision, .clean = T)
  
  logger::log_info("Simplifying network")
  
  # Simplify network graph
  simple_net <- 
    net %>%
    sfnetworks::activate("edges") %>%      
    dplyr::filter(!tidygraph::edge_is_multiple()) %>%          # multiple edges
    dplyr::filter(!tidygraph::edge_is_loop()) %>%              # removing loops
    tidygraph::convert(to_spatial_explicit, .clean = T)
  
  # check which node is the root node
  roots <- tidygraph::with_graph(simple_net, tidygraph::node_is_root())
  
  logger::log_info("Root node located at index: \n \n--> {which(roots == TRUE)}")
  logger::log_info("Adding points of interest to network")
  
  # blend LHD points into network
  blend_net <- 
    simple_net %>% 
    sfnetworks::st_network_blend(., points)  %>%
    # sfnetworks::st_network_blend(., fline_nodes)  %>% 
    activate("nodes") %>% 
    morph(to_subgraph, is.na(is_point)) %>%
    dplyr::mutate(is_point = FALSE) %>%
    unmorph()
  
  # blend_net %>% convert(to_spatial_subdivision, .clean = T)
  # check which node is the root node
  blend_roots <- tidygraph::with_graph(blend_net, tidygraph::node_is_root())
  
  logger::log_info("After adding points of interest, root node located at index: \n \n--> {which(blend_roots == TRUE)}")
  
  return(blend_net)
}

# create_network <- function(flines, pts) {
#   
#   logger::log_info("Creating network")
#   
#   # Network Graph
#   net <- 
#     flines %>% 
#     sfnetworks::as_sfnetwork() %>% 
#     tidygraph::convert(to_spatial_subdivision, .clean = T)
# 
#   logger::log_info("Simplifying network")
#   
#   # Simplify network graph
#   simple_net <- 
#     net %>%
#     sfnetworks::activate("edges") %>%      
#     dplyr::filter(!tidygraph::edge_is_multiple()) %>%          # multiple edges
#     dplyr::filter(!tidygraph::edge_is_loop()) %>%              # removing loops
#     tidygraph::convert(to_spatial_explicit, .clean = T)
#   
#   # check which node is the root node
#   roots <- tidygraph::with_graph(simple_net, tidygraph::node_is_root())
#   
#   logger::log_info("Root node located at index: \n \n--> {which(roots == TRUE)}")
#   
#   logger::log_info("Adding points of interest to network")
#   
#   # blend LHD points into network
#   blend_net <- 
#     simple_net %>% 
#     sfnetworks::st_network_blend(., fline_nodes)  %>% 
#     activate("nodes") %>% 
#     morph(to_subgraph, is.na(is_point)) %>%
#     dplyr::mutate(is_point = FALSE) %>%
#     unmorph()
#   
#   # blend_net %>% convert(to_spatial_subdivision, .clean = T)
#   # check which node is the root node
#   blend_roots <- tidygraph::with_graph(blend_net, tidygraph::node_is_root())
#   
#   logger::log_info("After adding points of interest, root node located at index: \n \n--> {which(blend_roots == TRUE)}")
#   
#   return(blend_net)
#   
# }
# Grouping function used in sfnetworks to segment network between LHDs/waterbodies
group_custom <- function(mode = "in") {
  # get the node indices of the nodes we want to route from:
  # - All points that were blended into network
  # - The root node at the start of network tree
  
  # Including the root, group all edges that DON'T have a blended point downstream
  origins = which(.N()$is_point | with_graph(.G(), node_is_root()))
  
  # Calculate the cost matrix from the origins, to all nodes in the network.
  costs <- st_network_cost(
    .G(), 
    from       = origins,
    Inf_as_NaN = TRUE,
    mode       = mode
  )
  
  # For each node in the network:
  # - Define which origins is first to be reached when travelling downstreams
  # - loop over columns & keep only min cost value per column
  # - first remove zeros, which are the cost values from & to the same node
  
  keep_minimum_cost <- function(i) {
    i[i == 0] = NaN
    if (any(!is.na(i))) i[i != min(i, na.rm = TRUE)] = NaN
    i
  }
  costs = apply(costs, 2, keep_minimum_cost)
  
  # for each origin:
  # - compute the paths to all nodes in its group.
  # - extract the edge indices that are part of these paths
  
  get_edge_indices <- function(i) {
    orig = origins[i]
    dest = which(!is.na(costs[i, ]))
    if (length(dest) > 0) {
      paths     = st_network_paths(
        .G(), 
        from = orig,
        to   = dest,
        mode = mode
      )
      edge_idxs = do.call(c, paths$edge_paths)
      unique(edge_idxs)
    }
  }
  
  groups = lapply(seq_len(nrow(costs)), get_edge_indices)
  
  # order groups by number of edges.
  groups = groups[order(lengths(groups), decreasing = TRUE)]
  
  # assign a group index to each edge
  edge_idxs = do.call(c, groups)
  group_idxs = rep(seq_along(groups), lengths(groups))
  # return the group indices in correct order
  # --> the order of the edges in the edge table of the network
  group_idxs[order(edge_idxs)]
}

# Find elevation of furthest downstream flowline in group of flowlines
fline_elevation <- function(flowline) {
  
  logger::log_info("Locating most downstream line segment")
  
  line_pos <-
    flowline %>% 
    # filter(group %in% c(221, 292)) %>% 
    # group_by(group) %>% 
    dplyr::arrange(topo_sort, .by_group = T) %>% 
    dplyr::slice_head()
  
  # ggplot() +  geom_sf(data = line_pos, aes(color = factor(topo_sort)))
  
  # network of mainstems
  network_lines  <-
    line_pos %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(5070) %>% 
    dplyr::group_by(group) %>%
    dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
    sf::st_line_merge() 
  
  # line segment endpoint
  line_end   <- 
    network_lines %>% 
    nhdplusTools::get_node("end") %>% 
    mutate(
      group    = network_lines$group
      # position = "end",
    )
  
  logger::log_info("Extracting elevation at flowline end point")
  
  # endpoint elevation
  end_point_elev <- elevatr::get_elev_point(locations = line_end)
  # mutate(group = line_pos$group)
  return(end_point_elev)
}

# Calculate distances between points (upstream and downstream)
# Return_wide will return a wide dataframe w/ a single row for each point where the network is split
network_connectivity <- function(flines, points, return_wide = TRUE) {
  
  # topo sort # for each comid --> LHD point
  lhd_sort <-
    flines %>%
    dplyr::filter(comid %in% points$comid) %>% 
    dplyr::select(comid, lhd_topo_sort = topo_sort, lhd_hydroseq = hydroseq) %>%
    # dplyr::select(from, to, comid, group, lhd_topo_sort = topo_sort, lhd_hydroseq = hydroseq) %>%
    dplyr::group_by(comid) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>% 
    sf::st_drop_geometry() 
  
  # Join comids and LHD ID to grouped line string
  lhd_info <-
    points %>% 
    dplyr::select(new_id, comid, reachcode,  geometry) %>%
    dplyr::mutate(comid = as.integer(comid)) %>% 
    dplyr::mutate(
      lng = sf::st_coordinates(.)[,1],
      lat = sf::st_coordinates(.)[,2]
    ) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::left_join(
      lhd_sort, 
      by = "comid"
    )
  
  logger::log_info("Calculating segmented network lengths")

  # edge_elev <-    
  #   edges %>% 
  #   group_by(group) %>% 
  #   fline_elevation()
  
  # get total lengths of grouped line segments
  group_sum <- 
    flines %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(
      min_topo_sort  = min(topo_sort)
    ) %>% 
    # ungroup() %>% arrange(min_topo_sort)  %>% group_by(group) %>%
    dplyr::summarise(
      from           = min(from),
      to             = max(to),
      min_topo_sort  = mean(min_topo_sort, na.rm = T)
    ) %>%
    dplyr::select(geometry, from, to, min_topo_sort, group) %>% 
    dplyr::mutate(length = as.numeric(sf::st_length(.))) %>% 
    sf::st_sf() %>%
    sf::st_cast("MULTILINESTRING") %>% 
    dplyr::mutate(
      length = round(length, 2)
    )

  logger::log_info("Finding relative US/DS position of segmented network")

  # match LHD points with upstream and downstream flowline groups
  lhd_group <- 
    points %>% 
    sf::st_buffer(1) %>%
    dplyr::select(new_id, comid, is_point, geometry) %>% 
    # dplyr::select(new_id, lhd_comid = comid, lhd_hydroseq = hydroseq, is_point, geometry) %>% 
    sf::st_filter(group_sum) %>% 
    sf::st_join(group_sum) %>% 
    sf::st_centroid()  %>% 
    dplyr::left_join(
      dplyr::select(lhd_info, new_id, lhd_topo_sort, hydroseq = lhd_hydroseq, lng, lat, reachcode),
      by = "new_id"
    ) %>% 
    # dplyr::left_join(dplyr::select(sf::st_drop_geometry(edge_elev), group, elevation),  by = "group") %>% 
    dplyr::relocate(comid, new_id, group, lhd_topo_sort,
                    from, to,
                    min_topo_sort, 
                    hydroseq, length, is_point, lng, lat, reachcode, geometry) %>% 
    dplyr::group_by(new_id) %>%
    dplyr::mutate(
      npts        = dplyr::n(),
      lag_from    = lag(from), 
      lag_to      = lag(to), 
      lead_from   = lead(from), 
      lead_to     = lead(to), 
      position    = dplyr::case_when(
        to   == lead_from                   ~ "us",
        from == lag_to                      ~ "ds",
        min_topo_sort == max(min_topo_sort) ~ "us",
        min_topo_sort == min(min_topo_sort) ~ "ds"
      )
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::relocate(comid, new_id, npts, group, 
                    from, to, lag_from, lag_to, lead_from, lead_to, position,length, 
                    hydroseq, lhd_topo_sort, min_topo_sort,  
                    is_point, lng, lat,reachcode, geometry) %>% 
    dplyr::select(-lag_from, -lag_to, -lead_from, -lead_to)
   
  logger::log_info("Creating list of upstream and downstream COMIDs")
  
  # list of comids per group 
  comid_group <- 
    flines %>%
    sf::st_drop_geometry() %>% 
    dplyr::select(group, comid) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(comid_list = list(as.character(comid)))
  
  # Join comids and create lists of COMIDs
  lhd_summary <-
    lhd_group %>% 
    sf::st_drop_geometry() %>%
    dplyr::select(group, new_id, comid, 
                  from, to, position, length, 
                  min_topo_sort, hydroseq, reachcode) %>%
    dplyr::group_by(comid, new_id, position) %>%
    dplyr::left_join(
      comid_group, 
      by = "group"
    ) %>% 
    dplyr::mutate(
      comid_list    = c(comid_list)
    ) %>% 
    dplyr::ungroup()
  
  # Nest groups as list 
  # nest_groups <-  lhd_summary %>% 
  #   dplyr::group_by(new_id) %>%  # dplyr::group_by(new_id, position) %>% 
  #   dplyr::summarise(group_list = list(as.character(group))) %>% dplyr::ungroup()
  
  # Join nested groups list column with rest of summary data
  # lhd_summary <- dplyr::left_join( lhd_summary, 
                                    # nest_groups, 
                                    # by = c("new_id")) 

  lhd_summary$geometry <- sf::st_geometry(lhd_group)

  lhd_summary <- 
    lhd_summary %>% 
    dplyr::select(new_id, group, from, to, position,
                  length, min_topo_sort, hydroseq, comid, reachcode, comid_list,
                  # group_list,
                  geometry) %>% 
    dplyr::group_by(new_id) %>%
    dplyr::arrange(length(comid_list), .by_group = T) %>% 
    dplyr::ungroup()
  # %>%  dplyr::select(new_id, from, to, position,  length,
  #         min_topo_sort, hydroseq, comid, reachcode, comid_list, group_list)
  
  logger::log_info("Tidying final dataframe...")
  
  # snake_net_name <- gsub(" ", "_", net_name)
  if(return_wide == TRUE) {
    
    logger::log_info("Returning wide dataframe...")
    
    # wide comid dataframe
    lhd_wide <-
      lhd_summary %>% 
      tidyr::pivot_wider(
        id_cols     = c(new_id, hydroseq, comid, reachcode, 
                        # group_list,
                        geometry),
        names_from  = "position",
        names_glue  = "{position}_{.value}",
        values_from = c(length, comid_list, group)
        # values_from = c(length, min_topo_sort, comid_list)
      ) %>% 
      dplyr::relocate(new_id, comid, hydroseq, us_length, ds_length, 
                      us_comid_list, ds_comid_list, us_group, ds_group, reachcode,
                      # group_list,
                      geometry)
    
     return(lhd_wide)
    
  } else if(return_wide == FALSE) {
    
    logger::log_info("Returning long dataframe...")
    
    return(lhd_summary)

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

