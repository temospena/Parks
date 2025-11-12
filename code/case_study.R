library(dplyr)
library(sf)
library(mapview)

# load files
parks_filtered = st_read("data/munich_parks_filtered.geojson")
roads = st_read("data/munich_roads.geojson")

# filter parks with area
parks_case = parks_filtered  |> 
  filter(area_m2 > 20000 & area_m2 < 200000)

# Speed-up: keep only roads that intersect parks before clipping
paths = roads[st_intersects(roads, st_union(parks_case), sparse = FALSE), ]
mapview(paths) + mapview(parks_case)
### MISSING THE ROADS AROUND PARKS EDGES? ####

### Remove trunks and highways (under parks)

# clip roads to parks with 20m buffer
paths = st_intersection(paths, st_buffer(parks_case, dist = 20)) |> 
  st_cast("LINESTRING")
paths = paths |> select(osm_id, name, highway, osm_id.1, geometry) |> 
  rename(park_osm_id = osm_id.1)
mapview(paths) + mapview(parks_case)
st_write(paths, "data/munich_park_paths_case.geojson", delete_dsn = TRUE)
         