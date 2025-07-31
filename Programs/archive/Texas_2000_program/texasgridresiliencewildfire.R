# weather_topology_assessment.R
# Topology-based resilience assessment under wildfire/temperature attacks
# Requirements: igraph, sf, dplyr, ggplot2, tidyr
# External: perseusWin.exe (for persistent homology)

# 1. Load libraries
library(igraph)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

# 2. Parameters
perseus_path <- "perseusWin.exe"  # path to Perseus binary
delta       <- 0.01
filt_len    <- 90
cap         <- 1

# 3. Data loading functions
# Avoid naming collision with base::data()
load_grid_data <- function(buses_file, branches_file,
                           bus_id_col = "BusNum",
                           from_col = "FromBus",
                           to_col   = "ToBus") {
  # Read CSVs
  buses_raw    <- read.csv(buses_file, stringsAsFactors = FALSE)
  branches_raw <- read.csv(branches_file, stringsAsFactors = FALSE)
  
  # Rename for igraph (must have 'name' in vertices and matching d\$from, d\$to)
  buses <- buses_raw %>% rename(name = all_of(bus_id_col))
  branches <- branches_raw %>%
    rename(from = all_of(from_col), to = all_of(to_col)) %>%
    mutate(from = as.character(from), to = as.character(to))
  
  # Construct undirected graph
  g <- graph_from_data_frame(d = branches, vertices = buses, directed = FALSE)
  return(list(graph = g, buses = buses, branches = branches))
}

# 4. Wildfire integration: point-in-polygon to select buses
get_impacted_buses <- function(buses_df, fire_shapefile,
                               lon_col = "Longitude", lat_col = "Latitude") {
  # fire_shapefile: a polygon shapefile (e.g. wildfire perimeter)
  fire_poly <- st_read(fire_shapefile, quiet = TRUE)
  
  # Convert buses to sf points
  buses_sf <- buses_df %>%
    filter(!is.na(.data[[lon_col]]), !is.na(.data[[lat_col]])) %>%
    st_as_sf(coords = c(lon_col, lat_col), crs = 4326)
  
  # Find which fall inside wildfire
  idx <- st_intersects(buses_sf, fire_poly)
  impacted_ids <- buses_sf$name[lengths(idx) > 0]
  return(as.character(impacted_ids))
}

# 5. Persistent homology (Betti 0 & 1) via Perseus
compute_persistence_perseus <- function(graph, perseus_path,
                                        delta, filt_len, cap) {
  # Compute graph distance matrix
  dmat <- distances(graph, mode = "all")
  # Normalize finite distances to [0,1]
  finite <- is.finite(dmat)
  dmat[finite] <- dmat[finite] / max(dmat[finite])
  
  # Write Perseus input
  sink("M.txt")
  cat(vcount(graph), "\n")
  cat(0, delta, filt_len, cap, "\n")
  apply(dmat, 1, function(r) cat(r, "\n"))
  sink()
  
  # Call Perseus
  system2(perseus_path, args = c("distmat", "M.txt", "Moutput"), stdout = TRUE)
  
  # Read Betti numbers
  betti <- as.matrix(read.table("Moutput_betti.txt"))
  colnames(betti) <- c("filtration","beta0","beta1")
  return(betti)
}

# 6. Compute normalized Betti changes
compute_betti_changes <- function(b0_orig, b0_att, b1_orig, b1_att) {
  L <- max(length(b0_orig), length(b1_orig),
           nrow(b0_att), nrow(b1_att))
  # pad or recycle
  b0o <- rep(b0_orig, length.out = L)
  b0a <- rep(b0_att[ , "beta0"], length.out = L)
  b1o <- rep(b1_orig, length.out = L)
  b1a <- rep(b1_att[ , "beta1"], length.out = L)
  
  delta_b0 <- sqrt(sum((b0a - b0o)^2)) / sqrt(sum(b0o^2))
  delta_b1 <- sqrt(sum((b1a - b1o)^2)) / sqrt(sum(b1o^2))
  return(c(delta_b0 = delta_b0, delta_b1 = delta_b1))
}

# 7. Motif metrics (triangles & 3-node chains)
compute_motif_metrics <- function(graph) {
  # Triangles: each counted three times by igraph
  tri   <- sum(count_triangles(graph)) / 3
  # Triplets: number of connected triples
  tplt  <- sum(choose(degree(graph), 2))
  chain <- tplt - tri
  return(list(triangles = tri,
              chains   = chain,
              conc_tri   = tri   / tplt,
              conc_chain = chain / tplt))
}

# 8. Simulation: progressive bus removals
simulate_attack <- function(graph, impacted, steps = NULL) {
  if (is.null(steps)) steps <- length(impacted)
  # Preallocate results
  res <- tibble(
    step          = 0,
    removed       = 0,
    delta_b0      = 0,
    delta_b1      = 0,
    tri_survival  = 1,
    chain_survival= 1,
    conc_tri      = NA,
    conc_chain    = NA
  )
  
  # Baseline
  betti_baseline <- compute_persistence_perseus(graph, perseus_path, delta, filt_len, cap)
  motif_base     <- compute_motif_metrics(graph)
  
  # Original Betti sequences
  b0_orig <- betti_baseline[ , "beta0"]
  b1_orig <- betti_baseline[ , "beta1"]
  
  for (i in seq_len(steps)) {
    node_rm <- impacted[i]
    g_att   <- delete_vertices(graph, node_rm)
    
    # Persistent homology
    betti_att <- compute_persistence_perseus(g_att, perseus_path, delta, filt_len, cap)
    bc        <- compute_betti_changes(b0_orig, betti_att, b1_orig, betti_att)
    
    # Motifs
    motif_att <- compute_motif_metrics(g_att)
    tri_s     <- motif_att$triangles / motif_base$triangles
    chn_s     <- motif_att$chains   / motif_base$chains
    
    # Record
    res <- add_row(res,
                   step           = i,
                   removed        = i,
                   delta_b0       = bc["delta_b0"],
                   delta_b1       = bc["delta_b1"],
                   tri_survival   = tri_s,
                   chain_survival = chn_s,
                   conc_tri       = motif_att$conc_tri,
                   conc_chain     = motif_att$conc_chain
    )
  }
  return(res)
}

# 9. Export for PowerWorld
export_for_powerworld <- function(buses_df, branches_df, prefix = "pw_export") {
  write.csv(buses_df,    paste0(prefix, "_buses.csv"),    row.names = FALSE)
  write.csv(branches_df, paste0(prefix, "_branches.csv"), row.names = FALSE)
}

# 10. MAIN: example usage
# --------------------------------------------
# 10.1 Load synthetic Texas grid (adjust filenames as needed)
grid_data <- load_grid_data(
  buses_file    = "case_ACTIVSg2000_buses.csv",
  branches_file = "case_ACTIVSg2000_branches.csv",
  bus_id_col    = "BusNum",
  from_col      = "FromBus",
  to_col        = "ToBus"
)

# Unpack graph and tables
gdl   <- grid_data$graph
buses <- grid_data$buses
br    <- grid_data$branches

# 10.2 Select impacted (wildfire) buses (example shapefile)
# wildfire_buses <- get_impacted_buses(buses, "fire_perimeter.shp",
#                                      lon_col="Longitude", lat_col="Latitude")
# For testing, take first 30 bus IDs
set.seed(123)
wildfire_buses <- sample(buses$name, 30)

# 10.3 Run attack simulation
attack_results <- simulate_attack(gdl, wildfire_buses)

# 10.4 Plot resilience metrics
plot_df <- attack_results %>%
  select(removed, delta_b0, delta_b1, tri_survival, chain_survival) %>%
  pivot_longer(-removed, names_to = "metric", values_to = "value")

ggplot(plot_df, aes(x = removed, y = value, color = metric)) +
  geom_line(size = 1) +
  labs(
    title = "Grid Resilience under Wildfire-Driven Bus Removals",
    x     = "Buses Removed",
    y     = "Metric Value",
    color = "Metric"
  ) +
  theme_minimal()

# 10.5 Export final scenario to CSV for PowerWorld
buses_final   <- buses   %>% filter(!name %in% wildfire_buses)
branches_final<- br      %>% filter(!from %in% wildfire_buses,
                                    !to   %in% wildfire_buses)
export_for_powerworld(buses_final, branches_final, prefix = "pw_scenario_fire")

# Note on Closure Errors:
# "object of type 'closure' is not subsettable" often occurs when you
# accidentally use a function name (like data or filter) as a variable,
# then try to subset it. Avoid naming variables with existing function names.