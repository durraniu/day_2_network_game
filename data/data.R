library(osmdata)
library(sf)

# Define cities and their bounding boxes
cities <- list(
  "New York" = c(-74.02, 40.70, -73.95, 40.75),
  "Amsterdam" = c(4.88, 52.35, 4.95, 52.39),
  "Toronto" = c(-79.40, 43.64, -79.34, 43.68),
  "Tokyo" = c(139.75, 35.67, 139.82, 35.71),
  "Windsor" = c(-83.05, 42.30, -82.95, 42.34)
)

# Function to get cycling network
get_cycle_network <- function(bbox) {
  cycles <- opq(bbox = bbox) |>
    add_osm_feature(key = "cycleway") |>
    osmdata_sf()
  
  # Also get bicycle designated roads
  bicycle_roads <- opq(bbox = bbox) |>
    add_osm_feature(key = "bicycle", value = "designated") |>
    osmdata_sf()
  
  # Combine both networks
  dplyr::bind_rows(cycles$osm_lines, 
                   bicycle_roads$osm_lines)

}

# Download and save cycling networks for all cities
cycle_networks <- list()
for (city in names(cities)) {
  cycle_networks[[city]] <- get_cycle_network(cities[[city]])
}

# Save the data
save(cycle_networks, file = here::here("posts/day2/data/cycle_networks.RData"))







get_road_network <- function(bbox) {
  # roads <- opq(bbox = bbox) |>
  #   add_osm_feature(key = "highway") |>
  #   osmdata_sf()
  roads <- opq(bbox = bbox) |>
    add_osm_feature(
      key = "highway",
      value = c("primary", "secondary", "tertiary")  # Main roads only
    ) |>
    osmdata_sf()
}


road_networks <- list()
for (city in names(cities)) {
  road_networks[[city]] <- get_road_network(cities[[city]])
}

# Save the data
save(road_networks, file = here::here("posts/day2/data/road_networks.RData"))
