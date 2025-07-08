snap_to_road <- \(pts) tibble(road_vid = st_nearest_feature(pts, road_ndx))

energy_sf  <- energy_sf  |> mutate(layer="energy")  |> bind_cols(snap_to_road(.))
telecom_sf <- telecom_sf |> mutate(layer="telecom") |> bind_cols(snap_to_road(.))

# optional social layer: schools from OSM
school_sf <- opq(st_bbox(target_area)) |>
  add_osm_feature("amenity", "school") |>
  osmdata_sf() |>
  purrr::pluck("osm_points") |>
  st_transform(4326) |>
  st_intersection(target_area) |>
  mutate(layer="social") |>
  bind_cols(snap_to_road(.))

points_sf <- bind_rows(energy_sf, telecom_sf, school_sf)

cat("Loaded:",
    nrow(points_sf),"points &", nrow(roads_sf),"road segments\n")