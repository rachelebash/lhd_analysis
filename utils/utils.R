# get NHDplus data within a specified buffer
# lhd_site: SF point 
# buffer_meters: buffer distance in meters
# Feature options: "flowline", "catchment", "outlet" or "all"
get_lhd_streamseg <- function(
  lhd_site, 
  buffer_meters = 201.168, 
  feature       = "flowline"
) {
  
  # Buffer each point with a 0.125 mile radius 
  buffer <- lhd_site %>% 
    sf::st_transform(5070) %>% 
    sf::st_buffer(201.168)
  
  miles_conversion <- buffer_meters/1609.344 
  
  logger::log_info("Buffering point: \n{buffer_meters} meters\n{miles_conversion} miles")
  
  logger::log_info("Getting NHDplus flowlines within buffer distance")
  
  # Use nhdplusTools package to retrieve NHD data
  flow_lines <- 
    buffer %>% 
    nhdplusTools::get_nhdplus(realization = feature)
  
  return(flow_lines)
}