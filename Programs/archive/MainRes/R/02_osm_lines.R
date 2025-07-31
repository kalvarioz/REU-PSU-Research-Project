osm_lines <- tryCatch({
  st_read(osm_file_path, layer = "lines", quiet = TRUE) |>
    st_transform(4326) |>
    st_intersection(target_area)
}, error = \(e) { warning("OSM read failed: ", e$message); NULL })