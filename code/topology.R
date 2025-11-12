# topology index for all parks
library(tidyverse)
library(sf)
library(sfnetworks)
library(tidygraph)
library(mapview)

# do first for 1, and then run for all
paths = st_read("data/munich_park_paths_case.geojson") 
nrow(distinct(paths, park_osm_id))
# give sequential id to each park
paths = paths |> mutate(park_id = as.integer(as.factor(park_osm_id)))
paths_case1 = paths |>  filter(park_id == 3)
mapview(paths_case1)

# create sfnetwork
edges = paths_case1 |> mutate(edge_id = c(1:n()))
nodes = edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))
nodes = nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy) # ignore warnings!

# Combine the node ID with the edge ID. 
source_nodes = nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes = nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)

# This may be unecessary, already done in qgis?
# Remove duplicate nodes. It may happen that more than one edge starts or finishes at a node. 
nodes = nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

# Create the sfnetwork object
graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

graph = graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))  # tem outro nome
  # mutate(length = st_length(geom))
  
# get values  - takes some time!
graph = graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>% 
  mutate(closeness = centrality_closeness(weights = length))

graph %>%  activate(nodes) %>%  as_tibble() %>% select(degree) %>% summary()
graph %>%  activate(nodes) %>%  as_tibble() %>% select(betweenness) %>% summary()
graph %>%  activate(nodes) %>%  as_tibble() %>% select(closeness) %>% summary() 

# check if makes sence -------------------------------------------------------

# # Plot betweenness of nodes
# ggplot() +
#   geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) + 
#   scale_colour_viridis_c(option = 'inferno') + scale_size_continuous(range = c(0,4)) + theme_classic()
# 
# # Plot closeness of nodes - sounds BAD
# ggplot() +
#   geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = closeness, size = closeness)) + 
#   scale_colour_viridis_c(option = 'inferno') + scale_size_continuous(range = c(0,4)) + theme_classic()
# 
# # Plot centrality degree of nodes
# ggplot() +
#   geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = degree, size = degree)) + 
#   scale_colour_viridis_c(option = 'inferno') + scale_size_continuous(range = c(0,4)) + theme_classic()

# export as sf ------------------------------------------------------------

centrality_nodes = graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf()
mapview(centrality_nodes, zcol = "betweenness")
mapview(centrality_nodes, zcol = "degree")
mapview(centrality_nodes, zcol = "closeness")

# with edges
centrality_edges = graph %>%
  activate(edges) %>%
  as_tibble() %>%
  st_as_sf()
mapview(centrality_edges, zcol = "betweenness")
#### CONTINUE HERE ####



# clean unnecessary nodes -------------------------------------------------

## NO BIG IMPROVEMENTS

# TESTE rede limpa osm targets
net = as_sfnetwork(paths_case1)

nodes_smoothed = convert(net, to_spatial_smooth) |> 
  activate(nodes) |> 
  as_tibble() |> 
  st_as_sf() |> 
  rename(nodeID = .tidygraph_node_index)

edges_smoothed = convert(net, to_spatial_smooth) |> 
  activate(edges) |> 
  as_tibble() |>
  select(park_id, osm_id, highway, geometry) |>
  mutate(edgeID = c(1:n())) |> 
  st_as_sf()

mapview::mapview(edges_smoothed, zcol="edgeID")
