library(dplyr)
library(sf)
library(mapview)
library(osmdata)


# City limit
city_districts <- st_read("data/munchen.geojson")
city_limit = st_union(city_districts) |> st_as_sf()
mapview(city_limit)
st_write(city_limit, "data/munich_limit.geojson", delete_dsn = TRUE)

# Extract OSM road network for Munich, mostly walking paths in parks
munich_roads = opq(bbox = st_bbox(city_limit)) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()
roads = munich_roads$osm_lines |>
  st_intersection(city_limit) |> 
  select(osm_id, name, highway, geometry) |> 
  # st_collection_extract("LINESTRING") |> 
  st_as_sf()
mapview(roads)

# Some roads are stored as polygons (like round loop paths), convert them to lines
roads_polygons = munich_roads$osm_polygons |>
  st_intersection(city_limit) |> 
  select(osm_id, name, highway, geometry) |> 
  st_as_sf() |> 
  st_cast("LINESTRING")
mapview(roads_polygons)
roads = rbind(roads |> st_cast("LINESTRING"),
              roads_polygons)
mapview(roads)

st_write(roads, "data/munich_roads.geojson", delete_dsn = TRUE)

# Extract OSM parks data for Munich
munich_parks = opq(bbox = st_bbox(city_limit)) |>
  add_osm_feature(key = "leisure", value = "park") |>
  osmdata_sf()
mapview(munich_parks$osm_polygons)
parks = munich_parks$osm_polygons |>
  st_intersection(city_limit) |> 
  filter(!leisure %in% c("pitch", "playground", "sports_centre")) |> 
  select(osm_id, name, leisure, landuse, geometry) |> 
  st_cast("GEOMETRYCOLLECTION") |> st_collection_extract("POLYGON") |> st_cast("POLYGON")
mapview(parks)
st_write(parks, "data/munich_parks.geojson",  delete_dsn = TRUE)



# shape infos

parks = parks |>
  mutate(
    area_m2 = st_area(geometry) |> round() |> units::drop_units(),
    area_ha = as.numeric(area_m2) / 10000 |> round(),
    perimeter_m = st_perimeter(geometry) |> round() |> units::drop_units(),
    shape_index = as.numeric(perimeter_m) / (2 * sqrt(pi * as.numeric(area_m2))),
    isoperimetric_ratio = ((perimeter_m^2) / (4 * pi * area_m2))
  )
mapview(parks, zcol = "shape_index")
mapview(parks, zcol = "isoperimetric_ratio")
mapview(parks |> filter(isoperimetric_ratio < 6) , zcol = "isoperimetric_ratio")

# plots graphs for ratios and shapes and areas
hist(parks$area_ha, main = "Histogram of Park Areas in Munich", xlab = "Area (ha)", breaks = 20)
hist(parks$shape_index, main = "Histogram of Park Shape Index in Munich", xlab = "Shape Index", breaks = 20)
hist(parks$isoperimetric_ratio, main = "Histogram of Park Isoperimetric", xlab = "Isoperimetric Ratio", breaks = 20)

# Filter parks based on area and isoperimetric ratio
parks_filtered = parks |>
  filter(area_ha >= 2) |> 
  filter(isoperimetric_ratio < 6)
mapview(parks_filtered, zcol = "isoperimetric_ratio")
st_write(parks_filtered, "data/munich_parks_filtered.geojson", delete_dsn = TRUE)

