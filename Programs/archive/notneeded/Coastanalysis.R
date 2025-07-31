library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(osmdata)
library(dplyr)

# --- Section 1: Define the Coastal Zone of Interest ---
cat("--- Section 1: Defining California Coastal Zone ---\n")

# Fetch map data for the USA
states_sf <- ne_states(country = "United States of America", returnclass = "sf")
california_sf <- states_sf[states_sf$name == "California", ]

# To accurately define the coast, we get the coastline data itself
coastline <- ne_coastline(scale = "large", returnclass = "sf")
# Clip the world coastline to the bounding box of California
ca_coastline <- st_intersection(coastline, st_buffer(california_sf, 10000)) # Buffer to ensure intersection

# Define the coastal buffer zone (e.g., 20 kilometers)
# We need to project the data to a CRS that uses meters for accurate buffering
ca_coastline_proj <- st_transform(ca_coastline, 3857) # Web Mercator projection
coastal_buffer <- st_buffer(ca_coastline_proj, dist = 20000) # 20km buffer
# Transform back to standard lat/lon for plotting
coastal_buffer_latlon <- st_transform(coastal_buffer, st_crs(california_sf))
cat("Defined a 20km coastal buffer zone for California.\n")


# --- Section 2: Load Infrastructure Data from a Local .osm File ---
cat("\n--- Section 2: Loading Infrastructure Data from Local .osm File ---\n")

# --- NEW METHOD: Reading from a local file ---
# Define the path to your .osm file. 
# IMPORTANT: You must change this placeholder to the actual path of your file.
osm_file_path <- "C:/Users/dinag/OneDrive/RESEARCHPROJECT - MAPPING/map.osm" 
osm_lines_from_file <- NULL

# Use a tryCatch block to handle cases where the file doesn't exist
tryCatch({
  # Read the 'lines' layer from your .osm file
  cat(paste("Attempting to read 'lines' layer from:", osm_file_path, "\n"))
  osm_raw_lines <- st_read(osm_file_path, layer = "lines")
  
  # Ensure the data is in the correct projection
  osm_lines_transformed <- st_transform(osm_raw_lines, st_crs(california_sf))
  
  # Filter the lines to include only those within our coastal buffer zone
  osm_lines_from_file <- st_intersection(osm_lines_transformed, coastal_buffer_latlon)
  
  cat(paste("Successfully loaded and filtered", nrow(osm_lines_from_file), "line features from the .osm file.\n"))
}, error = function(e) {
  cat(paste("Error reading .osm file:", e$message, "\n"))
  cat("Please ensure the file path is correct and the file is accessible.\n")
})


# --- OLD METHOD: Querying the live server (now commented out) ---
# cat("\n--- Section 2: Querying OpenStreetMap for Coastal Infrastructure ---\n")
# bbox <- st_bbox(coastal_buffer_latlon)
# q <- opq(bbox = bbox) %>%
#   add_osm_feature(key = 'highway', value = c('motorway', 'trunk', 'primary')) %>%
#   add_osm_feature(key = 'power', value = c('line', 'substation')) %>%
#   add_osm_feature(key = 'railway', value = 'rail')
# osm_data <- osmdata_sf(q)
# cat("Downloaded roads, railways, and power infrastructure from OSM.\n")


# --- Section 3: Load and Filter User's CSV Data to Coastal Zone ---
cat("\n--- Section 3: Loading and Filtering User Data to Coastal Zone ---\n")
all_points_list <- list()

# Process Energy Data
energy_df_raw <- read.csv("Power_Plants.csv")
names(energy_df_raw)[names(energy_df_raw) == "X"] <- "longitude"
names(energy_df_raw)[names(energy_df_raw) == "Y"] <- "latitude"
energy_df_raw <- energy_df_raw[!is.na(energy_df_raw$longitude) & !is.na(energy_df_raw$latitude),]
energy_sf_projected <- st_as_sf(energy_df_raw, coords = c("longitude", "latitude"), crs = 3857)
energy_sf_latlon <- st_transform(energy_sf_projected, st_crs(california_sf))
energy_coastal <- st_intersection(energy_sf_latlon, coastal_buffer_latlon)
if (nrow(energy_coastal) > 0) {
  all_points_list$energy <- st_sf(type = "Energy Plant", source = "User Data", geometry = energy_coastal$geometry)
}

# Process Telecom Data
telecom_df_raw <- read.csv("Antenna_Structure_Registration_(ASR).csv")
telecom_df_ca <- subset(telecom_df_raw, LocState == "CA")
telecom_df <- telecom_df_ca[, c("RegNum", "latdec", "londec")]
names(telecom_df) <- c("id", "latitude", "longitude")
telecom_df <- na.omit(telecom_df)
telecom_sf <- st_as_sf(telecom_df, coords=c("longitude", "latitude"), crs=4326)
telecom_coastal <- st_intersection(telecom_sf, coastal_buffer_latlon)
if (nrow(telecom_coastal) > 0) {
  telecom_coastal_sample <- telecom_coastal[sample(nrow(telecom_coastal), min(500, nrow(telecom_coastal))),]
  all_points_list$telecom <- st_sf(type = "Telecom Tower", source = "User Data", geometry = telecom_coastal_sample$geometry)
}


# Process Transport Data
transport_df_raw <- read.csv("NTAD_Amtrak_Stations_6759003720897441974.csv")
transport_df_ca <- subset(transport_df_raw, State == "CA")
transport_df <- transport_df_ca[, c("OBJECTID", "y", "x")]
names(transport_df) <- c("id", "latitude", "longitude")
transport_df <- na.omit(transport_df)
transport_sf <- st_as_sf(transport_df, coords=c("longitude", "latitude"), crs=4326)
transport_coastal <- st_intersection(transport_sf, coastal_buffer_latlon)
if (nrow(transport_coastal) > 0) {
  all_points_list$transport <- st_sf(type = "Amtrak Station", source = "User Data", geometry = transport_coastal$geometry)
}

# Combine all point data into a single sf data frame
all_points_sf <- do.call(rbind, all_points_list)
cat(paste("Filtered user data to", nrow(all_points_sf), "assets within the coastal zone.\n"))


# --- Section 4: Create the Composite Map ---
cat("\n--- Section 4: Generating the Composite Coastal Vulnerability Map ---\n")

map_plot <- ggplot() +
  # Layer 1: California state outline
  geom_sf(data = california_sf, fill = "antiquewhite", color = "grey40") +
  
  # Layer 2: The semi-transparent coastal buffer zone
  geom_sf(data = coastal_buffer_latlon, fill = "lightblue", alpha = 0.3, color = NA)

# --- Conditionally add line data from the .osm file ---
# This block checks if the osm_lines_from_file object was created successfully.
if (!is.null(osm_lines_from_file) && nrow(osm_lines_from_file) > 0) {
  
  # Filter for roads and add the layer if any exist
  roads <- osm_lines_from_file %>% filter(!is.na(highway))
  if (nrow(roads) > 0) {
    map_plot <- map_plot + geom_sf(data = roads, color = "grey60", size = 0.5)
  }
  
  # Filter for power lines and add the layer if any exist
  power_lines <- osm_lines_from_file %>% filter(!is.na(power))
  if (nrow(power_lines) > 0) {
    map_plot <- map_plot + geom_sf(data = power_lines, color = "gold3", size = 0.5)
  }
  
  # Filter for railways and add the layer if any exist
  railways <- osm_lines_from_file %>% filter(!is.na(railway))
  if (nrow(railways) > 0) {
    map_plot <- map_plot + geom_sf(data = railways, color = "darkred", linetype="dashed", size = 0.6)
  }
}

# Continue building the plot
map_plot <- map_plot +
  # Layer 4: Infrastructure points (from your data)
  geom_sf(data = all_points_sf, aes(color = type, shape = type), size = 2.5, alpha = 0.8) +
  
  # Customize appearance
  scale_color_manual(name = "Point Infrastructure", values = c("Energy Plant" = "tomato", "Telecom Tower" = "darkblue", "Amtrak Station" = "darkgreen")) +
  scale_shape_manual(name = "Point Infrastructure", values = c("Energy Plant" = 17, "Telecom Tower" = 16, "Amtrak Station" = 18)) +
  
  labs(
    title = "Critical Infrastructure in California's Coastal Zone",
    subtitle = "Displaying assets within a 20km coastal buffer from user data and a local .osm file",
    caption = "Data Sources: User-provided CSVs & Local .osm file"
  ) +
  
  # Set map limits to the coastal zone for focus
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
  theme_minimal()

print(map_plot)
cat("\nMap has been generated. Check the Plots pane.\n")
