roads_sf <- st_read(roads_shp, quiet = TRUE) |>
  st_transform(4326) |>
  st_intersection(target_area) |>
  st_collection_extract("LINESTRING") |>   # ⬅ keep only line parts
  st_cast("LINESTRING") |>                 # ⬅ MULTI → single
  st_simplify(500) |>
  st_make_valid() 

road_net <- as_sfnetwork(roads_sf, directed = FALSE) |>
  activate(edges) |>
  mutate(weight = edge_length())

road_ndx <- st_as_sf(road_net, "shape_length") |> st_geometry()