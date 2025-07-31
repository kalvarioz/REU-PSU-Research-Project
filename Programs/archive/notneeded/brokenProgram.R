cat("--- PHASE 1: Loading Libraries and Processing Data ---\n")
suppressPackageStartupMessages({
  library(sf)             # The modern standard for spatial data in R
  library(igraph)         # The core library for network analysis
  library(dplyr)          # Essential for data manipulation
  library(ggplot2)        # For static plotting (like the persistence diagram)
  library(TDA)            # For Topological Data Analysis (persistence diagrams)
  library(mapview)        # For creating interactive maps
  library(rnaturalearth)  # For geographic context (e.g., state outlines)
  library(rnaturalearthdata)
  library(tidyr)          # For data cleaning and reshaping
})

process_california_data <- function() {
  cat("-> Loading and processing California infrastructure data...\n")
  
  # Energy Nodes: Power Plants
  power_plants_sf <- read.csv("Power_Plant_3845005786177023144.csv") %>%
    rename(longitude = x, latitude = y) %>%
    drop_na(longitude, latitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    mutate(
      id = paste0("E_", OBJECTID),
      subtype = PriEnergySource,
      type = "Energy"
    ) %>%
    select(id, type, subtype, geometry)
  
  # Transport Nodes: Amtrak Stations
  amtrak_sf <- read.csv("NTAD_Amtrak_Stations_6759003720897441974.csv") %>%
    filter(State == "CA") %>%
    rename(longitude = lon, latitude = lat) %>%
    drop_na(longitude, latitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    mutate(
      id = paste0("R_", OBJECTID),
      subtype = StnType,
      type = "Transport"
    ) %>%
    select(id, type, subtype, geometry)
  
  # Telecom/Charging Nodes: MDHD Charging Stations
  charging_sf <- read.csv(
    "MDHD_Dashboard_ArcGIS_Updated_Nov_9045361474699528162.csv",
    stringsAsFactors = FALSE
  ) %>%
    # 1) rename so we have numeric lon/lat and a clean subtype column
    rename(
      longitude = Longitude,
      latitude  = Latitude,
      type   = `Charging or Hydrogen`
    ) %>%
    # 2) drop any rows missing coords
    drop_na(longitude, latitude) %>%
    # 3) turn into simple‐features 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    # 4) add your id and type columns
    mutate(
      id     = paste0("C_", OBJECTID),
      type   = "Charging"
    ) %>%
    select(id, type, subtype, geometry)
  
  # Transmission Lines (Placeholder for real geometry)
  transmission_sf <- read.csv("Electric_Transmission_Paths_(CEC).csv")
  if("SHAPE" %in% colnames(transmission_sf)){
    transmission_sf <- transmission_sf %>%
      st_as_sf(wkt = "SHAPE", crs = 4326) %>%
      mutate(id = paste0("TX_", OBJECTID)) %>%
      select(id, geometry)
  } else {
    cat("-> Warning: Transmission paths CSV missing geometry information.\n")
    transmission_sf <- NULL
  }
  
  # Combine all node types into a single 'sf' data frame for easier handling
  all_nodes_sf <- rbind(
    power_plants_sf,
    amtrak_sf,
    charging_sf
  )
  
  cat("-> Data loading and processing complete.\n")
  
  return(list(
    nodes_energy = power_plants_sf,
    nodes_transport = amtrak_sf,
    nodes_charging = charging_sf,
    edges_transmission = transmission_sf,
    all_nodes_sf = all_nodes_sf
  ))
}

# Execute the function
infra_data <- process_california_data()

# Combine all node types into a single 'sf' data frame for easier handling.
all_nodes_sf <- rbind(
  infra_data$nodes_energy,
  infra_data$nodes_transport,
  infra_data$nodes_telecom
)
cat("\n--- PHASE 2: Building the Expanded Multi-Layer Network ---\n")

cat("-> Creating Layer 1: Physical Transmission Grid\n")
transmission_endpoints <- st_cast(infra_data$edges_transmission, "POINT")
start_points <- st_as_sf(transmission_endpoints[c(TRUE, FALSE), ])
end_points <- st_as_sf(transmission_endpoints[c(FALSE, TRUE), ])

from_node_idx <- st_nearest_feature(start_points, infra_data$nodes_energy)
to_node_idx <- st_nearest_feature(end_points, infra_data$nodes_energy)

edges_transmission_df <- data.frame(
  from = infra_data$nodes_energy$id[from_node_idx],
  to = infra_data$nodes_energy$id[to_node_idx],
  relationship = "transmission"
)

# -- LAYER 2: Power Dependency (Energy-to-Other, Directed) --
# This layer connects each Telecom and Transport node to its single nearest Energy node.
cat("-> Creating Layer 2: Power Dependency\n")
energy_nodes <- infra_data$nodes_energy
dependent_nodes <- rbind(infra_data$nodes_charging, infra_data$nodes_transport)

# For each dependent node, find the index of the nearest energy source.
nearest_energy_idx <- st_nearest_feature(dependent_nodes, energy_nodes)

edges_dependency_df <- data.frame(
  from = energy_nodes$id[nearest_energy_idx], # Edge goes FROM Energy
  to = dependent_nodes$id,                    # TO the dependent node
  relationship = "power_dependency"
)

# -- LAYER 3: Geographical Proximity (Intra-layer) --
# Connects nodes of the SAME subtype if they are within a given radius.
# This represents localized interdependencies not captured by other layers.
cat("-> Creating Layer 3: Geographical Proximity\n")
proximity_threshold_km <- 25 # Define the proximity radius in kilometers.
proximity_threshold_m <- proximity_threshold_km * 1000

edges_proximity <- list()
# We loop through each unique SUBTYPE (e.g., Solar, Wind, Rail, Tower).
for (st in unique(all_nodes_sf$subtype)) {
  nodes_subset <- all_nodes_sf[all_nodes_sf$subtype == st, ]
  
  # Only proceed if there is more than one node of that subtype.
  if (nrow(nodes_subset) > 1) {
    # Use st_is_within_distance for an efficient proximity search.
    prox_matrix <- st_is_within_distance(nodes_subset, dist = proximity_threshold_m, sparse = FALSE)
    diag(prox_matrix) <- FALSE # A node isn't a neighbor to itself.
    
    edge_indices <- which(prox_matrix, arr.ind = TRUE)
    
    if (nrow(edge_indices) > 0) {
      edges_proximity[[st]] <- data.frame(
        from = nodes_subset$id[edge_indices[, 1]],
        to = nodes_subset$id[edge_indices[, 2]],
        relationship = "proximity"
      )
    }
  }
}
edges_proximity_df <- do.call(rbind, edges_proximity)


# --- Step 2.2: Combine All Layers and Create the igraph Object ---
cat("-> Assembling final graph...\n")
# Combine all the edge data frames we created.
all_edges_df <- rbind(edges_transmission_df, edges_dependency_df, edges_proximity_df) %>%
  filter(from != to) %>% # Ensure no self-loops
  distinct() # Remove any duplicate edges

# The vertices data frame for the graph.
nodes_for_graph <- as.data.frame(all_nodes_sf) %>% select(id, type, subtype)

# Create the final igraph object. We define it as directed.
net <- graph_from_data_frame(d = all_edges_df, vertices = nodes_for_graph, directed = TRUE)
cat("-> Multi-layer network constructed successfully.\n")
cat(paste("-> Graph Summary: ", vcount(net), "nodes,", ecount(net), "edges.\n"))


# ---
# PHASE 3: BASELINE TOPOLOGICAL & RESILIENCE ANALYSIS
# ---

cat("\n--- PHASE 3: Analyzing Baseline Network Resilience ---\n")

# --- Step 3.1: Calculate Baseline Betti Numbers (Topological Holes) ---
# Betti-0: Number of connected components. A resilient network has 1 component.
# Betti-1: Number of independent cycles/loops. Higher is more redundant.
# We analyze the network as undirected to understand its overall connectivity.
net_undirected <- as_undirected(net)
baseline_betti_0 <- length(decompose(net_undirected))
baseline_betti_1 <- ecount(net_undirected) - vcount(net_undirected) + baseline_betti_0
cat(paste("-> Baseline Betti-0 (Components):", baseline_betti_0, "\n"))
cat(paste("-> Baseline Betti-1 (Redundancy):", baseline_betti_1, "\n"))

# --- Step 3.2: Generate Persistence Diagram (TDA) ---
# This analyzes the "shape" of the data based on node locations,
# identifying clusters (0-dim features) and loops (1-dim features).
# Long bars in the resulting plot indicate robust spatial structures.
cat("-> Computing persistence diagram for spatial analysis...\n")
node_coords <- st_coordinates(all_nodes_sf)
# We use a ripsDiag which is a standard TDA algorithm.
# maxscale determines how "far out" we look for connections. We convert km to degrees approx.
max_scale_degrees <- 200 / 111 # Approx 200km in degrees
persistence_diagram <- ripsDiag(X = node_coords, maxdimension = 1, maxscale = max_scale_degrees)
cat("-> Persistence diagram computed.\n")


# ---
# PHASE 4: HAZARD & CASCADING FAILURE SIMULATION
# ---

cat("\n--- PHASE 4: Simulating Hazard Impact ---\n")

# --- Step 4.1: Define Hazard Zone (Modular Approach) ---
# Select ONE hazard to simulate by uncommenting the desired block.
# This makes it easy to switch between different scenarios.

HAZARD_TYPE <- "wildfire" # Change this to "coastal" or other custom types

define_hazard_zone <- function(type) {
  cat(paste("-> Defining hazard zone for scenario:", type, "\n"))
  
  # Get California state boundary for context
  ca_boundary <- ne_states(country = "United States of America", returnclass = "sf") %>%
    filter(name == "California")
  
  if (type == "coastal") {
    # SCENARIO 1: COASTAL HAZARD (e.g., sea-level rise, tsunami)
    coastline <- st_cast(ca_boundary, "LINESTRING")
    # Buffer the coastline by 20km to create the hazard zone.
    hazard_zone <- st_buffer(coastline, dist = 20000) # dist is in meters
    return(st_transform(hazard_zone, 4326)) # Ensure CRS matches nodes
    
  } else if (type == "wildfire") {
    # SCENARIO 2: HYPOTHETICAL WILDFIRE RISK ZONE
    # This is a placeholder polygon representing a high-risk area.
    # You could replace this with actual data from CalFire or similar sources.
    # Polygon covers parts of the Sierra Nevada foothills.
    wildfire_polygon <- st_polygon(list(matrix(c(
      -121.5, 38.0,
      -120.0, 37.5,
      -119.5, 39.0,
      -121.0, 39.5,
      -121.5, 38.0
    ), ncol = 2, byrow = TRUE)))
    hazard_zone <- st_sfc(wildfire_polygon, crs = 4326)
    return(hazard_zone)
    
  } else {
    stop("Unknown hazard type specified.")
  }
}

# Generate the hazard zone based on the selected type
hazard_buffer_sf <- define_hazard_zone(HAZARD_TYPE)

# Find all infrastructure nodes that fall within this hazard zone.
# These are our initial points of failure.
nodes_in_hazard_zone_sf <- st_filter(all_nodes_sf, hazard_buffer_sf)
initial_failures <- nodes_in_hazard_zone_sf$id
cat(paste("-> Identified", length(initial_failures), "initial failures within the", HAZARD_TYPE, "hazard zone.\n"))


# --- Step 4.2: Simulate Cascading Failures ---
# This function simulates how failures propagate through the network.
# It's based on a breadth-first search logic.
simulate_cascade <- function(graph, initial_failed_nodes) {
  if (length(initial_failed_nodes) == 0) {
    return(character(0))
  }
  
  # Queue of nodes whose failure needs to be processed.
  queue <- initial_failed_nodes
  # Set of all nodes that have failed so far.
  all_failed_nodes <- initial_failed_nodes
  
  while (length(queue) > 0) {
    # Get the next node to process from the front of the queue.
    current_node <- queue[1]
    queue <- queue[-1]
    
    # Check if the node actually exists in the graph.
    if (!current_node %in% V(graph)$name) next
    
    # Find all neighbors of the current failed node.
    # 'out' mode finds nodes that are dependent on the current_node.
    successors <- neighbors(graph, V(graph)[name == current_node], mode = "out")
    
    if (length(successors) > 0) {
      for (s in successors) {
        successor_name <- V(graph)$name[s]
        
        # If this successor hasn't already failed, it's a new failure.
        if (!successor_name %in% all_failed_nodes) {
          # Check the type of relationship. We only cascade over 'power_dependency'.
          edge_id <- E(graph)[from(V(graph)[name == current_node]) & to(s)]
          if (length(edge_id) > 0 && E(graph)$relationship[edge_id] == "power_dependency") {
            # Add to the set of all failed nodes.
            all_failed_nodes <- c(all_failed_nodes, successor_name)
            # Add to the queue to process its downstream effects.
            queue <- c(queue, successor_name)
          }
        }
      }
    }
  }
  return(unique(all_failed_nodes))
}

cat("-> Simulating cascading failures...\n")
total_failed_nodes <- simulate_cascade(net, initial_failures)
cat(paste("-> Simulation complete. Total nodes failed:", length(total_failed_nodes), "\n"))

# --- Step 4.3: Analyze Post-Failure Network ---
# Create the damaged network by removing all failed nodes.
net_damaged <- delete_vertices(net, V(net)[name %in% total_failed_nodes])

# Recalculate Betti numbers to measure the impact on connectivity and redundancy.
net_damaged_undirected <- as_undirected(net_damaged)
post_failure_betti_0 <- length(decompose(net_damaged_undirected))
post_failure_betti_1 <- ecount(net_damaged_undirected) - vcount(net_damaged_undirected) + post_failure_betti_0

cat(paste("-> Post-Failure Betti-0:", post_failure_betti_0, "\n"))
cat(paste("-> Post-Failure Betti-1:", post_failure_betti_1, "\n"))


# ---
# PHASE 5: OUTPUT GENERATION
# ---

cat("\n--- PHASE 5: Generating Final Outputs ---\n")

# --- Step 5.1: Summary Report ---
# Print a concise report to the console comparing the before and after states.
cat("\n--- RESILIENCE ANALYSIS REPORT ---\n")
cat(paste("Hazard Scenario Simulated:", HAZARD_TYPE, "\n"))
cat("------------------------------------\n")
cat(paste("Initial Nodes Failed (in Hazard Zone):", length(initial_failures), "\n"))
cat(paste("Cascading Failures (Dependencies):", length(total_failed_nodes) - length(initial_failures), "\n"))
cat(paste("Total Nodes Failed:", length(total_failed_nodes), "\n"))
cat("------------------------------------\n")
cat(paste("Network Components (Betti-0):", baseline_betti_0, "->", post_failure_betti_0, "\n"))
cat(paste("Network Redundancy (Betti-1):", baseline_betti_1, "->", post_failure_betti_1, "\n"))
cat("------------------------------------\n")

# --- Step 5.2: Plot Persistence Diagram ---
# This plot shows the inherent structural stability. Long bars = robust features.
# It is plotted in the 'Plots' pane in RStudio.
if (!is.null(persistence_diagram) && !is.null(persistence_diagram[["diagram"]])) {
  # Create a ggplot for better aesthetics
  diag_df <- as.data.frame(persistence_diagram[["diagram"]])
  colnames(diag_df) <- c("Dimension", "Birth", "Death")
  
  # We add a small amount to the death of infinite features for plotting
  max_death <- max(diag_df$Death[is.finite(diag_df$Death)])
  diag_df$Death[is.infinite(diag_df$Death)] <- max_death + 0.1 * max_death
  
  p_diag_plot <- ggplot(diag_df, aes(x = Birth, y = Death, color = as.factor(Dimension))) +
    geom_point(size=2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(title = "Persistence Diagram of Infrastructure Network",
         subtitle = "Spatial robustness of node locations",
         x = "Birth (Scale)",
         y = "Death (Scale)",
         color = "Dimension") +
    theme_minimal() +
    scale_color_manual(values = c("0" = "#F8766D", "1" = "#00BFC4"), labels=c("Components", "Loops"))
  
  print(p_diag_plot)
  cat("\n-> Persistence diagram plotted. See Plots pane.\n")
} else {
  cat("\n-> Could not generate persistence diagram. Check TDA inputs.\n")
}

# --- Step 5.3: Generate Interactive Map of Simulation ---
# Add a 'status' column to our nodes data for visualization.
all_nodes_sf$status <- "Operational"
all_nodes_sf$status[all_nodes_sf$id %in% initial_failures] <- "Initial Failure"
# Use setdiff to find nodes that failed only due to cascade.
cascaded_failures <- setdiff(total_failed_nodes, initial_failures)
all_nodes_sf$status[all_nodes_sf$id %in% cascaded_failures] <- "Cascaded Failure"

# Create a color palette for the map.
status_palette <- c("Operational" = "#2ECC71", "Initial Failure" = "#F39C12", "Cascaded Failure" = "#E74C3C")

# Create the map using mapview, which is great for exploration.
simulation_map <- mapview(hazard_buffer_sf, col.regions = "#F39C12", alpha.regions = 0.3, layer.name = "Hazard Zone") +
  mapview(
    filter(all_nodes_sf, status != "Operational"), # Only show failed nodes to reduce clutter
    zcol = "status",
    col.regions = status_palette,
    layer.name = "Infrastructure Failure Status"
  )

# Print the map object, which will render it in the 'Viewer' pane in RStudio.
print(simulation_map)
cat("-> Interactive simulation map generated. See Viewer pane.\n")


