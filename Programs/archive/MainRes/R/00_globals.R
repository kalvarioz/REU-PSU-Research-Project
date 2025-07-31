libs <- c("sf", "dplyr", "tidyr", "osmdata", "mapview",
          "sfnetworks", "igraph", "tidygraph", "dodgr")
suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))
mapviewOptions(fgb = TRUE)

## ---- user parameters ----
county_shp_path <- "D:/REU@PSU/California_County_Boundaries_8408091426384550881/cnty19_1.shp"
county_to_keep  <- "Santa Cruz"
buffer_km       <- 20

osm_file_path   <- "C:/Users/dinag/OneDrive/RESEARCHPROJECT - MAPPING/california-latest.osm.pbf"
plant_csv       <- "D:/REU@PSU/Databases/Power_Plants.csv"
tower_csv       <- "D:/REU@PSU/Databases/Antenna_Structure_Registration_(ASR).csv"
roads_shp       <- "D:/REU@PSU/California_All_Public_Roads_Network/California_All_Public_Roads_Network.shp"
haz_flood_gj    <- "D:/REU@PSU/Databases/06087C_20190710/S_FLD_HAZ_AR.shp"