energy_sf <- read.csv(plant_csv, stringsAsFactors = FALSE) |>
  rename(longitude = X, latitude = Y) |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) |>
  st_intersection(target_area)

telecom_sf <- read.csv(tower_csv, stringsAsFactors = FALSE) |>
  filter(LocState == "CA") |>
  select(id = RegNum, latitude = latdec, longitude = londec) |>
  drop_na() |>
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) |>
  st_intersection(target_area)

if (nrow(telecom_sf) > 500) telecom_sf <- telecom_sf[sample(nrow(telecom_sf),500),]

roads_sf <- st_read(roads_shp, quiet = TRUE) |>
  st_transform(4326) |>
  st_intersection(target_area) |>
  st_simplify(500)