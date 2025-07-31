

recompute_edge_attrs <- function(g, branch_data = branch_info) {
  
  if (ecount(g) == 0) return(g)
  
  # Get edge list from the graph
  el <- igraph::as_data_frame(g, what = "edges") %>%
    transmute(
      from_bus = as.integer(from),
      to_bus = as.integer(to),
      b1 = pmin(from_bus, to_bus),
      b2 = pmax(from_bus, to_bus)
    )
  
  # Join with branch data and ensure we maintain exact edge count
  joined <- left_join(el, branch_data, by = c("b1", "b2"))
  
  # Critical fix: Ensure we have exactly the same number of rows as edges
  if (nrow(joined) != nrow(el)) {
    warning("Join operation changed number of rows. This suggests duplicate entries in branch_data.")
    
    # Check for duplicates in branch_data
    branch_dupes <- branch_data %>%
      group_by(b1, b2) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if (nrow(branch_dupes) > 0) {
      message("Found ", nrow(branch_dupes), " duplicate entries in branch_data, removing duplicates")
      
      # Remove duplicates, keeping first occurrence
      branch_data_clean <- branch_data %>%
        distinct(b1, b2, .keep_all = TRUE)
      
      # Redo the join with cleaned data
      joined <- left_join(el, branch_data_clean, by = c("b1", "b2"))
    }
  }
  
  # Final safety check
  if (nrow(joined) != ecount(g)) {
    stop("Critical error: Number of joined rows (", nrow(joined), 
         ") doesn't match graph edges (", ecount(g), ")")
  }
  
  # Handle missing values with defaults
  joined <- joined %>%
    mutate(
      r = ifelse(is.na(r), 0.01, r),
      x = ifelse(is.na(x), 0.01, x)
    )
  
  # Set edge attributes
  E(g)$LineR <- joined$r
  E(g)$LineX <- joined$x
  E(g)$Y <- 1 / (joined$r + 1i * joined$x)
  E(g)$Yreal <- Re(E(g)$Y)
  
  return(g)
}

# ====================================================================
# ATTACK SIMULATION FUNCTIONS
# ====================================================================

simulate_attack <- function(g, buses_to_remove = NULL) {
  
  if (!is.null(buses_to_remove) && length(buses_to_remove) > 0) {
    # Convert to character and find valid vertices
    valid <- intersect(as.character(buses_to_remove), V(g)$name)
    
    if (length(valid) > 0) {
      g <- delete_vertices(g, valid)
    }
  }
  
  if (ecount(g) > 0) {
    g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
    g <- recompute_edge_attrs(g, branch_info)
  }
  
  return(g)
}

identify_deenergized_components <- function(g, generator_buses) {
  
  if (vcount(g) == 0) return(integer(0))
  
  comps <- components(g)
  generator_buses_char <- as.character(generator_buses)
  
  deenergized <- unlist(lapply(seq_along(comps$csize), function(idx) {
    component_nodes <- V(g)$name[comps$membership == idx]
    
    # If component has no generators, it becomes deenergized
    if (!any(component_nodes %in% generator_buses_char)) {
      return(component_nodes)
    } else {
      return(NULL)
    }
  }))
  
  return(as.integer(deenergized))
}

generate_attacked_state_matrix <- function(affected_bus_ids) {
  message("=== GENERATING ATTACKED STATE FROM ORIGINAL CSV ===")
  
  # Check if healthy state is loaded
  if (!exists("healthy_state_matrix")) {
    stop("Healthy state matrix not loaded. Run initialize_healthy_state_baseline() first.")
  }
  
  original_matrix <- healthy_state_matrix
  
  if (length(affected_bus_ids) == 0) {
    message("No buses affected - returning original matrix")
    return(original_matrix)
  }
  if (is.null(rownames(original_matrix)) || is.null(colnames(original_matrix))) {
    message("⚠️  Matrix lacks proper row/column names, using indices")
    # Assume matrix rows/cols correspond to bus indices 1:n
    n_buses <- nrow(original_matrix)
    bus_ids <- 1:n_buses
  } else {
    # Extract bus IDs from row names (they might be like "bus_123" or just "123")
    bus_ids <- as.numeric(gsub("bus_", "", rownames(original_matrix)))
  }
  
  # Find indices of buses that survived the attack
  surviving_buses <- setdiff(bus_ids, affected_bus_ids)
  surviving_indices <- which(bus_ids %in% surviving_buses)
  
  if (length(surviving_indices) < 2) {
    message("⚠️  Too few surviving buses (", length(surviving_indices), ") for analysis")
    # Return minimal 2x2 matrix to avoid Perseus errors
    return(matrix(c(0, 0.1, 0.1, 0), nrow = 2, ncol = 2))
  }
  
  # Extract submatrix for surviving buses
  attacked_matrix <- original_matrix[surviving_indices, surviving_indices]
  
  message("✓ Attacked matrix generated:")
  message("  Original: ", nrow(original_matrix), "×", ncol(original_matrix))
  message("  Attacked: ", nrow(attacked_matrix), "×", ncol(attacked_matrix))
  message("  Buses removed: ", length(affected_bus_ids))
  
  return(attacked_matrix)
}
prepare_fire_polygons_by_step <- function(fire_data) {
  
  if (is.null(fire_data) || nrow(fire_data) == 0) {
    message("No fire data provided")
    return(list())
  }
  
  # Ensure we have step information
  if (!"step" %in% names(fire_data)) {
    message("Warning: No 'step' column found, creating single step")
    fire_data$step <- 1
  }
  
  # Split fire data by step
  fire_polys_by_step <- split(fire_data, fire_data$step)
  
  # Remove any empty steps
  fire_polys_by_step <- fire_polys_by_step[lengths(fire_polys_by_step) > 0]
  
  message("Prepared fire polygons for ", length(fire_polys_by_step), " steps")
  
  # Print step summary
  for (i in seq_along(fire_polys_by_step)) {
    step_num <- names(fire_polys_by_step)[i]
    step_data <- fire_polys_by_step[[i]]
    total_area <- sum(step_data$fire_acres, na.rm = TRUE)
    message("  Step ", step_num, ": ", nrow(step_data), " polygons, ", 
            round(total_area), " acres")
  }
  
  return(fire_polys_by_step)
}
run_enhanced_fire_cascade <- function(graph, buses_sf, fire_data, buffer_km = 5, steps = 20) {
  
  message("=== ENHANCED FIRE CASCADE SIMULATION ===")
  
  # Validate inputs
  if (is.null(fire_data) || nrow(fire_data) == 0) {
    stop("No fire data provided")
  }
  
  if (is.null(buses_sf) || nrow(buses_sf) == 0) {
    stop("No bus spatial data provided")
  }
  
  if (vcount(graph) == 0) {
    stop("Empty graph provided")
  }
  
  # Prepare fire polygons by step
  message("Preparing fire data...")
  fire_polys_by_step <- prepare_fire_polygons_by_step(fire_data)
  
  if (length(fire_polys_by_step) == 0) {
    stop("No valid fire polygon steps found")
  }
  
  # Run the simulation
  message("Starting cascade simulation...")
  result <- simulate_fire_cascade(
    graph = graph,
    buses_sf = buses_sf, 
    fire_polys_by_step = fire_polys_by_step,
    buffer_km = buffer_km,
    steps = steps
  )
  
  return(result)
}
validate_fire_data <- function(fire_data) {
  
  if (is.null(fire_data)) {
    return(list(valid = FALSE, error = "Fire data is NULL"))
  }
  
  if (nrow(fire_data) == 0) {
    return(list(valid = FALSE, error = "Fire data is empty"))
  }
  
  if (!inherits(fire_data, "sf")) {
    return(list(valid = FALSE, error = "Fire data is not an sf object"))
  }
  
  # Check for required columns
  required_cols <- c("attr_IncidentName", "fire_intensity")
  missing_cols <- required_cols[!required_cols %in% names(fire_data)]
  
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE, 
      warning = paste("Missing recommended columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  # Check geometry validity
  invalid_geom <- !st_is_valid(fire_data)
  if (any(invalid_geom)) {
    return(list(
      valid = TRUE,
      warning = paste(sum(invalid_geom), "invalid geometries found - will be fixed automatically")
    ))
  }
  
  return(list(valid = TRUE, message = "Fire data validation passed"))
}

#' Validate bus spatial data
validate_bus_data <- function(buses_sf) {
  
  if (is.null(buses_sf)) {
    return(list(valid = FALSE, error = "Bus data is NULL"))
  }
  
  if (nrow(buses_sf) == 0) {
    return(list(valid = FALSE, error = "Bus data is empty"))
  }
  
  if (!inherits(buses_sf, "sf")) {
    return(list(valid = FALSE, error = "Bus data is not an sf object"))
  }
  
  # Check for required columns
  if (!"bus_i" %in% names(buses_sf)) {
    return(list(valid = FALSE, error = "Bus data missing 'bus_i' column"))
  }
  
  # Check for coordinate data
  coords_available <- !all(is.na(st_coordinates(buses_sf)))
  if (!coords_available) {
    return(list(valid = FALSE, error = "Bus data has no valid coordinates"))
  }
  
  return(list(valid = TRUE, message = "Bus data validation passed"))
}

# ====================================================================
# FIRE CASCADE SIMULATION - FIXED VERSION
# ====================================================================

simulate_fire_cascade <- function(graph, buses_sf, fire_polys_by_step, buffer_km = 5, steps = 20) {
  
  message("=== STARTING FIRE CASCADE SIMULATION ===")
  message("Parameters:")
  message("  Buffer distance: ", buffer_km, " km")
  message("  Maximum steps: ", steps)
  message("  Fire data steps: ", length(fire_polys_by_step))
  
  # Initialize tracking variables
  graphs <- list(graph)
  buses_lost_per_step <- list()
  fire_points_list <- list()
  step_metrics <- list()
  
  # Get generator buses for deenergization analysis
  generator_buses <- tryCatch({
    bus_info %>%
      filter(total_gen > 0) %>%
      pull(bus_i)
  }, error = function(e) {
    message("Warning: Could not identify generator buses: ", e$message)
    return(integer(0))
  })
  
  message("Identified ", length(generator_buses), " generator buses")
  
  # Simulate each time step
  actual_steps <- min(steps, length(fire_polys_by_step))
  message("Running ", actual_steps, " simulation steps")
  
  for (step_num in seq_len(actual_steps)) {
    current_graph <- graphs[[step_num]]
    
    message("--- Step ", step_num, " ---")
    
    tryCatch({
      # Get current fire perimeter
      if (length(fire_polys_by_step) >= step_num && !is.null(fire_polys_by_step[[step_num]])) {
        current_fire_data <- fire_polys_by_step[[step_num]]
        
        # Find buses within fire perimeter using existing function
        if (nrow(current_fire_data) > 0 && nrow(buses_sf) > 0) {
          fire_impact_result <- find_buses_near_wildfire(current_fire_data, buses_sf, buffer_km)
          fire_affected_buses <- fire_impact_result$affected_buses
          
          # Create fire points for tracking with impact type information
          burning_buses_sf <- buses_sf %>%
            filter(bus_i %in% fire_affected_buses) %>%
            mutate(
              impact_type = case_when(
                bus_i %in% fire_impact_result$direct_contact ~ "direct",
                bus_i %in% fire_impact_result$proximity_contact ~ "buffer",
                TRUE ~ "unknown"
              ),
              fire_step = step_num
            )
          
          fire_points_list[[step_num]] <- burning_buses_sf
          
          message("  Fire impact: ", length(fire_affected_buses), " buses affected")
          message("    Direct: ", length(fire_impact_result$direct_contact))
          message("    Buffer: ", length(fire_impact_result$proximity_contact))
        } else {
          fire_affected_buses <- integer(0)
          fire_points_list[[step_num]] <- buses_sf[0, ] %>%
            mutate(impact_type = character(0), fire_step = integer(0))
          message("  No fire impact this step")
        }
      } else {
        fire_affected_buses <- integer(0)
        fire_points_list[[step_num]] <- buses_sf[0, ] %>%
          mutate(impact_type = character(0), fire_step = integer(0))
        message("  No fire data for step ", step_num)
      }
      
      # Remove fire-affected buses
      g_after_fire <- simulate_attack(current_graph, fire_affected_buses)
      message("  Graph after fire removal: ", vcount(g_after_fire), " buses, ", ecount(g_after_fire), " edges")
      
      # Identify and remove deenergized components
      deenergized_buses <- identify_deenergized_components(g_after_fire, generator_buses)
      message("  Cascade failures: ", length(deenergized_buses), " buses")
      
      # Remove deenergized buses
      g_final <- simulate_attack(g_after_fire, deenergized_buses)
      message("  Final graph: ", vcount(g_final), " buses, ", ecount(g_final), " edges")
      
      # Store results
      graphs[[step_num + 1]] <- g_final
      buses_lost_this_step <- unique(c(fire_affected_buses, deenergized_buses))
      buses_lost_per_step[[step_num]] <- buses_lost_this_step
      
      # Calculate step metrics
      step_metrics[[step_num]] <- calculate_step_metrics(g_final, buses_lost_this_step, step_num, 
                                                         fire_affected_buses, deenergized_buses)
      
      # Break if grid is completely destroyed
      if (vcount(g_final) == 0) {
        message("Grid completely destroyed at step ", step_num)
        break
      }
      
    }, error = function(e) {
      message("Error in fire cascade simulation at step ", step_num, ": ", e$message)
      # Continue with current graph
      graphs[[step_num + 1]] <- current_graph
      buses_lost_per_step[[step_num]] <- integer(0)
      fire_points_list[[step_num]] <- buses_sf[0, ] %>%
        mutate(impact_type = character(0), fire_step = integer(0))
      step_metrics[[step_num]] <- calculate_step_metrics(current_graph, integer(0), step_num, 
                                                         integer(0), integer(0))
    })
  }
  
  # Pad empty results if needed
  while (length(fire_points_list) <= steps) {
    fire_points_list[[length(fire_points_list) + 1]] <- buses_sf[0, ] %>%
      mutate(impact_type = character(0), fire_step = integer(0))
  }
  
  # Combine metrics with error handling
  metrics_df <- tryCatch({
    if (length(step_metrics) > 0) {
      bind_rows(step_metrics)
    } else {
      data.frame()
    }
  }, error = function(e) {
    message("Error combining metrics: ", e$message)
    data.frame()
  })
  
  message("=== FIRE CASCADE SIMULATION COMPLETE ===")
  message("Total simulation steps: ", length(graphs) - 1)
  message("Final grid size: ", vcount(graphs[[length(graphs)]]), " buses")
  
  return(list(
    graphs = graphs,
    buses_lost_per_step = buses_lost_per_step,
    fire_points_list = fire_points_list,
    metrics = metrics_df
  ))
}

# FIXED: Function with correct function name and implementation
find_buses_near_wildfire <- function(fire_data, buses_sf, proximity_km = 5) {
  
  # Direct contact with fire perimeters
  direct_hits <- st_within(buses_sf, fire_data)
  direct_contact <- buses_sf$bus_i[lengths(direct_hits) > 0]
  
  # Proximity contact with fire centers
  proximity_contact <- integer(0)
  
  # Check if fire has center coordinates
  fires_with_centers <- fire_data %>%
    filter(has_center_point) %>%
    st_drop_geometry()
  
  if (nrow(fires_with_centers) > 0) {
    fire_centers_sf <- fires_with_centers %>%
      st_as_sf(coords = c("attr_InitialLongitude", "attr_InitialLatitude"), crs = 4326)
    
    # Create buffer (convert km to degrees approximately)
    buffer_deg <- proximity_km / 111
    fire_buffers <- st_buffer(fire_centers_sf, dist = buffer_deg)
    
    # Find buses in buffer
    buffer_hits <- st_within(buses_sf, fire_buffers)
    proximity_contact <- buses_sf$bus_i[lengths(buffer_hits) > 0]
  }
  
  # Combine and return all affected buses
  affected_buses <- unique(c(direct_contact, proximity_contact))
  
  return(list(
    affected_buses = affected_buses,
    direct_contact = direct_contact,
    proximity_contact = proximity_contact,
    total_affected = length(affected_buses)
  ))
}

# CORE FUNCTION: Generate power matrix for affected buses only
generate_post_cascade_power_matrix <- function(graph_original, cascade_results, step_to_analyze) {
  
  # Get current graph state at specified step
  if (step_to_analyze > length(cascade_results$graphs)) {
    stop("Requested step ", step_to_analyze, " but only ", length(cascade_results$graphs), " steps available")
  }
  
  current_graph <- cascade_results$graphs[[step_to_analyze]]
  active_buses <- as.numeric(V(current_graph)$name)
  
  if (length(active_buses) < 2) {
    stop("Insufficient active buses (", length(active_buses), ") for TDA analysis")
  }
  
  # Calculate power differences between active buses
  current_power <- bus_info %>%
    filter(bus_i %in% active_buses) %>%
    mutate(net_power = total_gen - load_mw) %>%
    select(bus_i, net_power)
  
  n_buses <- nrow(current_power)
  power_diff_matrix <- matrix(0, n_buses, n_buses)
  
  # Pairwise power differences
  for (i in 1:n_buses) {
    for (j in 1:n_buses) {
      if (i != j) {
        power_i <- current_power$net_power[i]
        power_j <- current_power$net_power[j]
        power_diff_matrix[i, j] <- abs(power_i - power_j)
      }
    }
  }
  
  # Normalize 
  if (max(power_diff_matrix) > 0) {
    power_diff_matrix <- power_diff_matrix / max(power_diff_matrix)
  }
  
  # Convert to perseus_V3 format
  colnames(power_diff_matrix) <- paste0("bus_", current_power$bus_i)
  result <- data.table(bus_i = current_power$bus_i)
  for (i in seq_len(ncol(power_diff_matrix))) {
    result[[colnames(power_diff_matrix)[i]]] <- power_diff_matrix[, i]
  }
  
  return(result)
}
debug_cascade_setup <- function(graph, buses_sf, fire_data, buffer_km = 5) {
  
  cat("=== FIRE CASCADE DEBUG INFORMATION ===\n")
  
  # Graph information
  cat("Graph Information:\n")
  cat("  Vertices:", vcount(graph), "\n")
  cat("  Edges:", ecount(graph), "\n")
  cat("  Is connected:", is_connected(graph), "\n\n")
  
  # Bus information
  cat("Bus Spatial Data:\n")
  cat("  Number of buses:", nrow(buses_sf), "\n")
  cat("  Has coordinates:", !all(is.na(st_coordinates(buses_sf))), "\n")
  cat("  CRS:", st_crs(buses_sf)$input, "\n\n")
  
  # Fire data information
  cat("Fire Data:\n")
  if (is.null(fire_data)) {
    cat("  ERROR: Fire data is NULL\n")
    return()
  }
  
  cat("  Number of polygons:", nrow(fire_data), "\n")
  cat("  Fire name:", unique(fire_data$attr_IncidentName)[1], "\n")
  cat("  Intensity levels:", paste(unique(fire_data$fire_intensity), collapse = ", "), "\n")
  cat("  Has step column:", "step" %in% names(fire_data), "\n")
  
  if ("step" %in% names(fire_data)) {
    step_counts <- table(fire_data$step)
    cat("  Steps available:", paste(names(step_counts), collapse = ", "), "\n")
    cat("  Polygons per step:", paste(as.numeric(step_counts), collapse = ", "), "\n")
  }
  
  cat("  Total area:", round(sum(fire_data$fire_acres, na.rm = TRUE)), "acres\n")
  cat("  CRS:", st_crs(fire_data)$input, "\n")
  cat("  Geometry valid:", all(st_is_valid(fire_data)), "\n\n")
  
  # Test fire-bus intersection
  cat("Fire-Bus Intersection Test:\n")
  tryCatch({
    test_result <- find_buses_near_wildfire(fire_data, buses_sf, buffer_km)
    cat("  Buses in direct contact:", length(test_result$direct_contact), "\n")
    cat("  Buses in buffer zone:", length(test_result$proximity_contact), "\n")
    cat("  Total affected buses:", length(test_result$affected_buses), "\n")
  }, error = function(e) {
    cat("  ERROR in intersection test:", e$message, "\n")
  })
  
  cat("\n=== END DEBUG INFORMATION ===\n")
}

message("✓ Fire data preparation functions loaded")
# ====================================================================
# METRICS CALCULATION - UPDATED FOR COMPATIBILITY
# ====================================================================

calculate_step_metrics <- function(graph, buses_lost, step_num, fire_affected = integer(0), deenergized = integer(0)) {
  
  # Basic graph properties
  n_vertices <- vcount(graph)
  n_edges <- ecount(graph)
  
  if (n_vertices == 0) {
    return(data.frame(
      step = step_num,
      vertices_remaining = 0,
      edges_remaining = 0,
      largest_component = 0,
      n_components = 0,
      load_served_pct = 0,
      gen_served_pct = 0,
      algebraic_connectivity = 0,
      avg_node_strength = 0,
      buses_lost_count = length(buses_lost),
      fire_affected = length(fire_affected),
      deenergized = length(deenergized),
      direct_hits = 0,
      buffer_hits = 0,
      total_lost = length(buses_lost),
      cascade_ratio = 0
    ))
  }
  
  # Component analysis
  comps <- components(graph)
  largest_component <- max(comps$csize)
  n_components <- comps$no
  
  # Power system metrics - updated to work with reorganized bus_info
  active_buses <- as.numeric(V(graph)$name)
  bus_status <- tryCatch({
    bus_info %>%
      mutate(is_active = bus_i %in% active_buses)
  }, error = function(e) {
    message("Warning: Could not access bus info for metrics: ", e$message)
    return(data.frame(load_mw = 0, total_gen = 0, is_active = FALSE))
  })
  
  total_load <- sum(bus_status$load_mw, na.rm = TRUE)
  total_gen <- sum(bus_status$total_gen, na.rm = TRUE)
  load_served <- sum(bus_status$load_mw[bus_status$is_active], na.rm = TRUE)
  gen_served <- sum(bus_status$total_gen[bus_status$is_active], na.rm = TRUE)
  
  load_served_pct <- if (total_load > 0) 100 * load_served / total_load else 0
  gen_served_pct <- if (total_gen > 0) 100 * gen_served / total_gen else 0
  
  # Network topology metrics
  if (n_edges > 0) {
    tryCatch({
      # Node strength (weighted degree)
      W <- as.matrix(as_adjacency_matrix(graph, attr = "Yreal", sparse = FALSE))
      strength <- rowSums(abs(W))
      avg_node_strength <- mean(strength)
      
      # Algebraic connectivity (second smallest eigenvalue of Laplacian)
      L <- diag(rowSums(abs(W))) - W
      eigenvals <- eigen(L, symmetric = TRUE, only.values = TRUE)$values
      eigenvals <- sort(eigenvals)
      algebraic_connectivity <- if (length(eigenvals) > 1) eigenvals[2] else 0
    }, error = function(e) {
      message("Warning: Could not compute network metrics: ", e$message)
      avg_node_strength <- 0
      algebraic_connectivity <- 0
    })
  } else {
    avg_node_strength <- 0
    algebraic_connectivity <- 0
  }
  
  # Enhanced metrics for cascade analysis
  fire_count <- length(fire_affected)
  cascade_count <- length(deenergized)
  total_lost <- length(buses_lost)
  
  # Calculate cascade ratio (how many cascade failures per fire impact)
  cascade_ratio <- if (fire_count > 0) cascade_count / fire_count else 0
  
  return(data.frame(
    step = step_num,
    vertices_remaining = n_vertices,
    edges_remaining = n_edges,
    largest_component = largest_component,
    n_components = n_components,
    load_served_pct = load_served_pct,
    gen_served_pct = gen_served_pct,
    algebraic_connectivity = algebraic_connectivity,
    avg_node_strength = avg_node_strength,
    buses_lost_count = total_lost,
    fire_affected = fire_count,
    deenergized = cascade_count,
    direct_hits = fire_count,  # Simplified for now
    buffer_hits = 0,           # Can be enhanced later
    total_lost = total_lost,
    cascade_ratio = cascade_ratio
  ))
}

# Enhanced cascade controller with proper function signature
CascadeController <- R6::R6Class("CascadeController",
                                 public = list(
                                   status = "idle",
                                   progress = 0,
                                   result = NULL,
                                   
                                   run_cascade = function(graph, buses_sf, fire_polys_by_step, fire_data, buffer_km, steps) {
                                     self$status <- "running"
                                     self$progress <- 0
                                     
                                     tryCatch({
                                       self$progress <- 20
                                       
                                       # Use the enhanced wrapper function instead
                                       result <- run_enhanced_fire_cascade(
                                         graph = graph,
                                         buses_sf = buses_sf,
                                         fire_data = fire_data,  # Use raw fire_data instead of fire_polys_by_step
                                         buffer_km = buffer_km,
                                         steps = steps
                                       )
                                       
                                       self$progress <- 100
                                       self$result <- result
                                       self$status <- "completed"
                                       
                                     }, error = function(e) {
                                       self$status <- "error"
                                       self$result <- list(error = e$message)
                                     })
                                     
                                     return(self$get_status())
                                   },
                                   
                                   get_status = function() {
                                     return(list(status = self$status, progress = self$progress, result = self$result))
                                   }
                                 )
)
# Create global cascade controller
if (!exists("cascade_controller")) {
  cascade_controller <<- CascadeController$new()
}

message("✓ main_attack_prototype_database.R fixed and updated for compatibility")
