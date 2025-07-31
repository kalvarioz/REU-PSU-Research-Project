cnty_sf <- st_read(county_shp_path, quiet = TRUE) |>
  st_make_valid()
stopifnot("COUNTY_NAM" %in% names(cnty_sf))

target_cnty <- cnty_sf %>% filter(COUNTY_NAM == county_to_keep)
stopifnot(nrow(target_cnty) > 0)

target_area <- if (buffer_km > 0) {
  target_cnty |> st_transform(3857) |> st_buffer(buffer_km*1000) |> st_transform(4326)
} else target_cnty

area_label <- paste0(county_to_keep,
                     if (buffer_km>0) paste0(" County (",buffer_km," km buffer)") else " County")
cat("Target area set to:", area_label, "\n")