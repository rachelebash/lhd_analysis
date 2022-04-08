# get data


#census_api_key("69c22a44cb77d733f583c3934266da10c914f128", install = TRUE)

#population census block data
get_pop <- function(geography, variables, state, year, geometry = TRUE) {
  pop <- tidycensus::get_decennial(geography = geography, 
                       variables = variables,
                       state = state,
                       year = year,
                       geometry = TRUE)
  tidy_pop <- pop %>%
    janitor::clean_names() %>%
    mutate(county = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 3),
           block_group = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 1),
           census_tract = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 2),
           state = sapply(strsplit(pop$NAME, ',\\s*'), `[`, 4),
           population = value) %>%
    select(state, county, block_group, census_tract, population, geoid, geometry)
}

#list of states surrounding CO for population census block data
all_states <- c("04", "08", "20", "31", "35", "40", "49", "56")