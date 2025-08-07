library(future)
library(future.apply)
library(parallel)
library(data.table)
library(foreach)
library(doParallel)
# =================================================================================================
# AttackAndCascade.R


# Brandon Calvario


# =================================================================================================


monitor_parallel_performance <- function() {
  tryCatch({
    message("=== PARALLEL STATUS CHECK ===")
    message("Current plan: ", class(future::plan())[1])
    message("Workers: ", future::nbrOfWorkers())
    
    gc_info <- gc(verbose = FALSE)
    total_memory <- sum(gc_info[, 2])
    message("Memory usage: ", round(total_memory, 1), " MB")
    
    conn_count <- monitor_connections() # Uses the global function
    if (conn_count > 10) {
      message("Warning: High connection count (", conn_count, ")")
    }
    
  }, error = function(e) {
    message("Error in performance monitoring: ", e$message)
  })
  
  return(invisible(TRUE))
}


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
  
  if (vcount(g) == 0) {
    message("Warning: Empty graph provided to simulate_attack")
    return(g)
  }
  
  if (!is.null(buses_to_remove) && length(buses_to_remove) > 0) {
    
    # Ensure vertex names exist
    if (is.null(V(g)$name)) {
      V(g)$name <- as.character(1:vcount(g))
    }
    
    # Convert to character and find valid vertices
    buses_to_remove_char <- as.character(buses_to_remove)
    valid_vertices <- intersect(buses_to_remove_char, V(g)$name)
    
    if (length(valid_vertices) > 0) {
      message("  Removing ", length(valid_vertices), " buses from graph")
      
      tryCatch({
        g <- delete_vertices(g, valid_vertices)
      }, error = function(e) {
        message("Error removing vertices: ", e$message)
        return(g)  # Return original graph if removal fails
      })
    } else {
      message("  No valid buses to remove found in graph")
    }
  }
  
  # Clean up and recompute attributes
  if (ecount(g) > 0) {
    g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
    
    tryCatch({
      g <- recompute_edge_attrs(g, branch_info)
    }, error = function(e) {
      message("Warning: Could not recompute edge attributes: ", e$message)
    })
  }
  
  return(g)
}

identify_deenergized_components <- function(g, generator_buses) {
  if (vcount(g) == 0) return(integer(0))
  
  if (is.null(V(g)$name)) {
    message("Warning: Graph vertices have no names, using vertex indices")
    V(g)$name <- as.character(1:vcount(g))
  }
  
  tryCatch({
    comps <- components(g)
    generator_buses_char <- as.character(generator_buses)
    deenergized <- c()
    
    for (comp_id in seq_len(comps$no)) {
      vertices_in_comp <- which(comps$membership == comp_id)
      if (length(vertices_in_comp) == 0) next
      
      component_nodes <- V(g)$name[vertices_in_comp]
      component_nodes <- component_nodes[!is.na(component_nodes)]
      if (length(component_nodes) == 0) next
      
      has_generator <- any(component_nodes %in% generator_buses_char)
      if (!has_generator) {
        deenergized <- c(deenergized, component_nodes)
      }
    }
    
    deenergized_int <- as.integer(unique(deenergized))
    deenergized_int <- deenergized_int[!is.na(deenergized_int)]
    
    if (length(deenergized_int) > 0) {
      message("  Component analysis: ", length(deenergized_int), " buses deenergized")
    }
    
    return(deenergized_int)
    
  }, error = function(e) {
    message("Error in component analysis: ", e$message)
    return(integer(0))
  })
}

generate_attacked_state_matrix <- function(affected_bus_ids) {
  message("=== GENERATING ATTACKED STATE FROM ORIGINAL MATRIX ===")
  
  # Check if healthy state is loaded
  if (!exists("healthy_state_matrix")) {
    stop("Original healthy state matrix not loaded. Run initialize_healthy_state_baseline() first.")
  }
  
  original_matrix <- healthy_state_matrix
  
  if (length(affected_bus_ids) == 0) {
    message("No buses affected - returning original matrix")
    return(original_matrix)
  }
  
  # **CRITICAL FIX**: Don't generate new matrix, use submatrix of original
  if (is.null(rownames(original_matrix))) {
    bus_ids <- 1:nrow(original_matrix)
  } else {
    bus_ids <- as.numeric(gsub("bus_", "", rownames(original_matrix)))
  }
  
  # Find indices of buses that survived the attack
  surviving_buses <- setdiff(bus_ids, affected_bus_ids)
  surviving_indices <- which(bus_ids %in% surviving_buses)
  
  if (length(surviving_indices) < 2) {
    message("⚠️  Too few surviving buses (", length(surviving_indices), ") for analysis")
    return(matrix(c(0, 0.1, 0.1, 0), nrow = 2, ncol = 2))
  }
  
  # **KEY FIX**: Extract submatrix from original (no normalization!)
  attacked_matrix <- original_matrix[surviving_indices, surviving_indices]
  
  message("✓ Attacked matrix extracted from ORIGINAL:")
  message("  Original: ", nrow(original_matrix), "×", ncol(original_matrix))
  message("  Attacked: ", nrow(attacked_matrix), "×", ncol(attacked_matrix))
  message("  Buses removed: ", length(affected_bus_ids))
  message("  Value range preserved: [", round(min(attacked_matrix, na.rm = TRUE), 6), 
          ", ", round(max(attacked_matrix, na.rm = TRUE), 6), "]")
  
  return(attacked_matrix)
}

prepare_fire_polygons_by_period <- function(fire_data) {
  message("=== ENHANCED FIRE POLYGON PREPARATION (REAL WFIGS DATA) ===")
  
  if (is.null(fire_data) || nrow(fire_data) == 0) {
    message("No fire data provided")
    return(list())
  }
  
  # Ensure all required columns exist (these should have been created by Global.R processing)
  required_columns <- c("time_period_quality", "period_from", "period_to", 
                        "period_duration_hours", "is_gap_period")
  
  missing_columns <- setdiff(required_columns, names(fire_data))
  
  if (length(missing_columns) > 0) {
    message("Missing time period columns, running enhanced processing...")
    message("Note: This should have been done in Global.R data processing")
    fire_data <- add_date_and_step_info(fire_data)
  }
  
  # Analyze data quality for summary
  if ("time_period_quality" %in% names(fire_data)) {
    quality_summary <- table(fire_data$time_period_quality)
    message("Time period data quality summary:")
    for (quality in names(quality_summary)) {
      message("  ", quality, " quality: ", quality_summary[quality], " records")
    }
  }
  
  # Handle multi-state fires - group by both fire name AND state for cross-state fires
  if ("is_multi_state_event" %in% names(fire_data) && any(fire_data$is_multi_state_event, na.rm = TRUE)) {
    message("Detected multi-state fires, using enhanced grouping...")
    fire_polys_by_period <- split(fire_data, 
                                  paste(fire_data$attr_IncidentName, 
                                        fire_data$attr_POOState, 
                                        fire_data$step, sep = "_"))
  } else {
    # Standard grouping by fire name and step
    fire_polys_by_period <- split(fire_data, 
                                  paste(fire_data$attr_IncidentName, 
                                        fire_data$step, sep = "_"))
  }
  
  # Remove empty periods
  fire_polys_by_period <- fire_polys_by_period[lengths(fire_polys_by_period) > 0]
  
  # Sort by time period with enhanced sorting for mixed quality data
  if (length(fire_polys_by_period) > 0) {
    period_times <- sapply(fire_polys_by_period, function(x) {
      if ("period_from" %in% names(x) && any(!is.na(x$period_from))) {
        min(x$period_from, na.rm = TRUE)
      } else {
        Sys.time()
      }
    })
    fire_polys_by_period <- fire_polys_by_period[order(period_times)]
  }
  
  message("Prepared fire polygons for ", length(fire_polys_by_period), " time periods")
  
  # Enhanced period summary showing data quality
  for (i in seq_along(fire_polys_by_period)) {
    period_data <- fire_polys_by_period[[i]]
    fire_name <- unique(period_data$attr_IncidentName)[1]
    step_num <- unique(period_data$step)[1]
    total_area <- sum(period_data$fire_acres, na.rm = TRUE)
    
    # Data quality indicators
    quality <- if ("time_period_quality" %in% names(period_data)) {
      quality_modes <- names(sort(table(period_data$time_period_quality), decreasing = TRUE))
      quality_modes[1]
    } else {
      "unknown"
    }
    
    has_ics209 <- "attr_ICS209RptForTimePeriodFrom" %in% names(period_data) && 
      any(!is.na(period_data$attr_ICS209RptForTimePeriodFrom))
    
    start_time <- if ("period_from" %in% names(period_data)) {
      min(period_data$period_from, na.rm = TRUE)
    } else {
      Sys.time()
    }
    
    end_time <- if ("period_to" %in% names(period_data)) {
      max(period_data$period_to, na.rm = TRUE)
    } else {
      Sys.time() + hours(24)
    }
    
    duration <- as.numeric(difftime(end_time, start_time, units = "hours"))
    
    is_gap <- if ("is_gap_period" %in% names(period_data)) {
      any(period_data$is_gap_period %in% TRUE, na.rm = TRUE)
    } else {
      FALSE
    }
    
    # Enhanced logging
    data_source <- if (has_ics209) "[ICS209]" else "[FALLBACK]"
    gap_indicator <- if (is_gap) " [GAP]" else ""
    
    message(sprintf("  Period %d: %s (Step %d) %s - %s to %s (%.1fh) - %d polygons, %.0f acres [%s]%s",
                    i, fire_name, step_num, data_source,
                    format(start_time, "%m/%d %H:%M"),
                    format(end_time, "%m/%d %H:%M"),
                    duration,
                    nrow(period_data),
                    total_area,
                    quality,
                    gap_indicator))
  }
  
  return(fire_polys_by_period)
}

# 3. ENHANCED TIME PERIOD VALIDATION
validate_time_periods <- function(fire_data) {
  message("Validating time periods for cascade analysis...")
  validation_results <- list()
  
  # Only validate if columns exist (should have been created by Global.R)
  if ("time_period_quality" %in% names(fire_data)) {
    # Check for fires with no time period data
    no_time_data <- fire_data %>%
      filter(time_period_quality == "low") %>%
      distinct(attr_IncidentName)
    
    if (nrow(no_time_data) > 0) {
      validation_results$warnings <- c(
        validation_results$warnings,
        paste("Fires with poor time data:", paste(no_time_data$attr_IncidentName, collapse = ", "))
      )
    }
    
    # Count fires by quality
    quality_by_fire <- fire_data %>%
      group_by(attr_IncidentName) %>%
      summarise(
        primary_quality = names(sort(table(time_period_quality), decreasing = TRUE))[1],
        .groups = 'drop'
      )
    
    quality_summary <- table(quality_by_fire$primary_quality)
    validation_results$info <- c(
      validation_results$info,
      paste("Fire quality distribution:", paste(names(quality_summary), "=", quality_summary, collapse = ", "))
    )
  }
  
  if ("period_duration_hours" %in% names(fire_data)) {
    long_periods <- fire_data %>%
      filter(period_duration_hours > 720) %>% # >30 days
      distinct(attr_IncidentName, step, .keep_all = TRUE)
    
    if (nrow(long_periods) > 0) {
      validation_results$warnings <- c(
        validation_results$warnings,
        paste("Fires with very long periods (>30 days):", nrow(long_periods), "periods found")
      )
    }
    
    # Check for reasonable period durations
    reasonable_periods <- fire_data %>%
      filter(period_duration_hours >= 1, period_duration_hours <= 168) %>% # 1 hour to 1 week
      nrow()
    
    validation_results$info <- c(
      validation_results$info,
      paste("Records with reasonable period durations:", reasonable_periods, "/", nrow(fire_data))
    )
  }
  
  # Check for fires with only one period
  if ("step" %in% names(fire_data) && "attr_IncidentName" %in% names(fire_data)) {
    single_period_fires <- fire_data %>%
      group_by(attr_IncidentName) %>%
      summarise(period_count = max(step, na.rm = TRUE), .groups = 'drop') %>%
      filter(period_count == 1)
    
    multi_period_fires <- fire_data %>%
      group_by(attr_IncidentName) %>%
      summarise(period_count = max(step, na.rm = TRUE), .groups = 'drop') %>%
      filter(period_count > 1)
    
    if (nrow(single_period_fires) > 0) {
      validation_results$info <- c(
        validation_results$info,
        paste("Fires with single time period:", nrow(single_period_fires), "fires")
      )
    }
    
    if (nrow(multi_period_fires) > 0) {
      validation_results$info <- c(
        validation_results$info,
        paste("Fires with multiple time periods:", nrow(multi_period_fires), "fires (good for temporal analysis)")
      )
    }
  }
  
  # Check for ICS209 data availability
  if ("attr_ICS209RptForTimePeriodFrom" %in% names(fire_data)) {
    fires_with_ics209 <- fire_data %>%
      group_by(attr_IncidentName) %>%
      summarise(
        has_ics209 = any(!is.na(attr_ICS209RptForTimePeriodFrom) & !is.na(attr_ICS209RptForTimePeriodTo)),
        .groups = 'drop'
      )
    
    ics209_count <- sum(fires_with_ics209$has_ics209)
    total_fires <- nrow(fires_with_ics209)
    
    validation_results$info <- c(
      validation_results$info,
      paste("Fires with ICS209 time data:", ics209_count, "/", total_fires)
    )
  }
  
  validation_results$valid <- TRUE
  
  # Log validation results
  if (length(validation_results$warnings) > 0) {
    message("Validation warnings:")
    for (warning in validation_results$warnings) {
      message("  ⚠ ", warning)
    }
  }
  
  if (length(validation_results$info) > 0) {
    message("Validation info:")
    for (info in validation_results$info) {
      message("  ℹ ", info)
    }
  }
  
  return(validation_results)
}

run_enhanced_fire_cascade <- function(graph, buses_sf, fire_data, buffer_km = 5, steps = 20, 
                                      use_parallel = TRUE, parallel_method = "multisession") {
  
  if (use_parallel) {
    message("=== STARTING ENHANCED PARALLEL CASCADE ANALYSIS ===")
    n_cores <- setup_parallel_processing(method = parallel_method)
    
    # UPDATED: Use enhanced fire polygon preparation
    fire_polys_by_period <- prepare_fire_polygons_by_period(fire_data)  # Use new function
    
    # Validate time periods
    validation_result <- validate_time_periods(fire_data)  # Use new function
    if (length(validation_result$warnings) > 0) {
      message("Time period warnings: ", paste(validation_result$warnings, collapse = "; "))
    }
    
    result <- simulate_fire_cascade(
      graph = graph,
      buses_sf = buses_sf,
      fire_polys_by_step = fire_polys_by_period,
      buffer_km = buffer_km,
      steps = steps
    )
    
    # Add enhanced metadata
    result$time_period_validation <- validation_result
    result$is_multi_state_analysis <- length(unique(fire_data$attr_POOState)) > 1
    result$states_analyzed <- unique(fire_data$attr_POOState)
    
    message("=== ENHANCED CASCADE ANALYSIS COMPLETE ===")
    return(result)
    
  } else {
    # Fallback to sequential processing with enhanced features
    message("Using enhanced sequential processing")
    fire_polys_by_period <- prepare_fire_polygons_by_period(fire_data)
    return(simulate_fire_cascade(graph, buses_sf, fire_polys_by_period, buffer_km, steps))
  }
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
# FIRE CASCADE SIMULATION - FIXED VERSION WITH SYNTAX CORRECTIONS
# ====================================================================

simulate_fire_cascade <- function(graph, buses_sf, fire_polys_by_step, buffer_km = 5, steps = 20) {
  
  message("=== PARALLEL FIRE CASCADE SIMULATION ===")
  message("Parameters:")
  message("  Buffer distance: ", buffer_km, " km")
  message("  Maximum steps: ", steps)
  message("  Fire data steps: ", length(fire_polys_by_step))
  message("  Parallel workers: ", future::nbrOfWorkers())
  
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
  
  # Determine which steps can be parallelized
  actual_steps <- min(steps, length(fire_polys_by_step))
  message("Running ", actual_steps, " simulation steps")
  
  # PARALLEL APPROACH 1: Parallel fire impact analysis for all steps
  message("Phase 1: Parallel fire impact analysis...")
  
  fire_impact_results <- future_lapply(seq_len(actual_steps), function(step_num) {
    
    if (length(fire_polys_by_step) >= step_num && !is.null(fire_polys_by_step[[step_num]])) {
      current_fire_data <- fire_polys_by_step[[step_num]]
      
      if (nrow(current_fire_data) > 0 && nrow(buses_sf) > 0) {
        # Ensure required columns exist
        required_cols <- c("attr_IncidentName", "fire_intensity", "fire_acres")
        missing_cols <- setdiff(required_cols, names(current_fire_data))
        
        for (col in missing_cols) {
          if (col == "attr_IncidentName") current_fire_data[[col]] <- "Unknown Fire"
          else if (col == "fire_intensity") current_fire_data[[col]] <- "Unknown"
          else if (col == "fire_acres") current_fire_data[[col]] <- 0
        }
        
        # Run fire impact analysis
        fire_impact_result <- find_buses_near_wildfire(current_fire_data, buses_sf, buffer_km)
        
        return(list(
          step = step_num,
          fire_affected_buses = fire_impact_result$affected_buses,
          fire_impact_data = fire_impact_result,
          fire_data = current_fire_data
        ))
      }
    }
    
    return(list(
      step = step_num,
      fire_affected_buses = integer(0),
      fire_impact_data = NULL,
      fire_data = NULL
    ))
    
  }, future.seed = TRUE)
  
  message("Phase 2: Sequential cascade propagation...")
  
  # SEQUENTIAL APPROACH: Step-by-step cascade (must be sequential due to dependencies)
  for (step_num in seq_len(actual_steps)) {
    current_graph <- graphs[[step_num]]
    
    message("--- Step ", step_num, " ---")
    
    tryCatch({
      # Get pre-computed fire impact results
      step_fire_result <- fire_impact_results[[step_num]]
      fire_affected_buses <- step_fire_result$fire_affected_buses
      
      # Create fire points with enhanced attribution
      if (length(fire_affected_buses) > 0 && !is.null(step_fire_result$fire_impact_data)) {
        burning_buses_sf <- buses_sf %>%
          filter(bus_i %in% fire_affected_buses) %>%
          mutate(
            impact_type = case_when(
              bus_i %in% step_fire_result$fire_impact_data$direct_contact ~ "direct",
              bus_i %in% step_fire_result$fire_impact_data$proximity_contact ~ "buffer",
              TRUE ~ "unknown"
            ),
            fire_step = step_num,
            is_compound_impact = step_fire_result$fire_impact_data$is_compound_event,
            affecting_fire_count = step_fire_result$fire_impact_data$compound_fire_count,
            affecting_fire_names = paste(step_fire_result$fire_impact_data$fire_names, collapse = ", ")
          )
        
        fire_points_list[[step_num]] <- burning_buses_sf
        
        message("  Fire impact: ", length(fire_affected_buses), " buses affected")
      } else {
        fire_points_list[[step_num]] <- buses_sf[0, ] %>%
          mutate(
            impact_type = character(0), 
            fire_step = integer(0),
            is_compound_impact = logical(0),
            affecting_fire_count = integer(0),
            affecting_fire_names = character(0)
          )
        message("  No fire impact this step")
      }
      
      # Remove fire-affected buses
      g_after_fire <- simulate_attack(current_graph, fire_affected_buses)
      message("  Graph after fire removal: ", vcount(g_after_fire), " buses, ", ecount(g_after_fire), " edges")
      
      # PARALLEL: Identify deenergized components
      deenergized_buses <- identify_deenergized_components(g_after_fire, generator_buses)
      message("  Cascade failures: ", length(deenergized_buses), " buses")
      
      # Remove deenergized buses
      g_final <- simulate_attack(g_after_fire, deenergized_buses)
      message("  Final graph: ", vcount(g_final), " buses, ", ecount(g_final), " edges")
      
      # Store results
      graphs[[step_num + 1]] <- g_final
      buses_lost_this_step <- unique(c(fire_affected_buses, deenergized_buses))
      buses_lost_per_step[[step_num]] <- buses_lost_this_step
      
      # PARALLEL: Calculate step metrics
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
        mutate(
          impact_type = character(0), 
          fire_step = integer(0),
          is_compound_impact = logical(0),
          affecting_fire_count = integer(0),
          affecting_fire_names = character(0)
        )
      step_metrics[[step_num]] <- calculate_step_metrics(current_graph, integer(0), step_num, 
                                                         integer(0), integer(0))
    })
  }
  
  # PARALLEL: Combine metrics
  metrics_df <- tryCatch({
    if (length(step_metrics) > 0) {
      do.call(rbind, step_metrics)
    } else {
      data.frame()
    }
  }, error = function(e) {
    message("Error combining metrics: ", e$message)
    data.frame()
  })
  
  message("=== PARALLEL FIRE CASCADE SIMULATION COMPLETE ===")
  message("Total simulation steps: ", length(graphs) - 1)
  message("Final grid size: ", vcount(graphs[[length(graphs)]]), " buses")
  message("Parallel efficiency: Used ", future::nbrOfWorkers(), " workers")
  
  return(list(
    graphs = graphs,
    buses_lost_per_step = buses_lost_per_step,
    fire_points_list = fire_points_list,
    metrics = metrics_df
  ))
}

# FIXED VERSION of find_buses_near_wildfire with proper syntax
find_buses_near_wildfire <- function(fire_data, buses_sf, proximity_km = 5) {
  message("=== FIRE-BUS INTERSECTION ANALYSIS ===")
  if (!inherits(fire_data, "sf")) stop("fire_data must be an sf object")
  if (!inherits(buses_sf, "sf")) stop("buses_sf must be an sf object")
  
  is_compound <- "is_compound_event" %in% names(fire_data) && any(fire_data$is_compound_event, na.rm = TRUE)
  unique_fires <- length(unique(fire_data$attr_IncidentName))
  message("Processing ", nrow(fire_data), " fire polygons from ", unique_fires, " fire event(s)")
  if (is_compound) message("COMPOUND EVENT: Multiple simultaneous fires detected")
  
  message("Validating geometries...")
  if (any(!st_is_valid(fire_data))) {
    message("  Fixing ", sum(!st_is_valid(fire_data)), " invalid fire geometries")
    fire_data <- st_make_valid(fire_data)
  }
  if (any(!st_is_valid(buses_sf))) {
    message("  Fixing ", sum(!st_is_valid(buses_sf)), " invalid bus geometries")
    buses_sf <- st_make_valid(buses_sf)
  }
  if (!st_crs(fire_data)$input == st_crs(buses_sf)$input) {
    message("  Transforming fire data CRS to match buses")
    fire_data <- st_transform(fire_data, st_crs(buses_sf))
  }
  
  message("Direct contact analysis...")
  intersections <- st_intersects(buses_sf, fire_data)
  direct_contact_indices <- which(lengths(intersections) > 0)
  direct_contact <- buses_sf$bus_i[direct_contact_indices]
  
  fire_names_affecting_direct <- unique(unlist(lapply(direct_contact_indices, function(i) {
    fire_indices <- intersections[[i]]
    if (length(fire_indices) > 0) return(fire_data$attr_IncidentName[fire_indices])
    return(character(0))
  })))
  
  message("  Direct contact found: ", length(direct_contact), " buses")
  if (length(direct_contact) > 0) {
    message("    Fires causing direct impact: ", paste(fire_names_affecting_direct, collapse = ", "))
  }
  
  message("Proximity contact analysis...")
  proximity_contact <- integer(0)
  if ("has_center_point" %in% names(fire_data)) {
    fires_with_centers <- fire_data %>%
      filter(has_center_point == TRUE, !is.na(attr_InitialLatitude), !is.na(attr_InitialLongitude))
    
    message("  Fires with center coordinates: ", nrow(fires_with_centers))
    if (nrow(fires_with_centers) > 0) {
      fire_centers_sf <- fires_with_centers %>%
        st_as_sf(coords = c("attr_InitialLongitude", "attr_InitialLatitude"), crs = st_crs(buses_sf))
      
      buffer_distance <- if (st_is_longlat(buses_sf)) proximity_km * 1000 else proximity_km / 111
      
      fire_buffers <- st_buffer(fire_centers_sf, dist = buffer_distance)
      buffer_intersections <- st_intersects(buses_sf, fire_buffers)
      proximity_indices <- which(lengths(buffer_intersections) > 0)
      proximity_contact <- buses_sf$bus_i[proximity_indices]
      message("  Proximity contact found: ", length(proximity_contact), " buses")
    }
  } else {
    message("  No fire center point data available for proximity analysis")
  }
  
  affected_buses <- unique(c(direct_contact, proximity_contact))
  result <- list(
    affected_buses = affected_buses, direct_contact = direct_contact,
    proximity_contact = proximity_contact, total_affected = length(affected_buses),
    fire_names = unique(fire_data$attr_IncidentName), is_compound_event = is_compound,
    compound_fire_count = unique_fires
  )
  
  message("=== INTERSECTION ANALYSIS COMPLETE ===")
  message("Total affected buses: ", length(affected_buses))
  return(result)
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
  
  # Use parallel processing for power system metrics calculation
  tryCatch({
    # Get basic graph properties
    n_vertices <- vcount(graph)
    n_edges <- ecount(graph)
    
    if (n_vertices == 0) {
      return(create_default_metrics(step_num, buses_lost, fire_affected, deenergized))
    }
    
    # PARALLEL: Component analysis and power metrics
    parallel_metrics <- future({
      
      # Component analysis
      comps <- components(graph)
      largest_component <- if (length(comps$csize) > 0) max(comps$csize) else 0
      n_components <- if (!is.null(comps$no)) comps$no else 1
      
      # Power system metrics
      active_buses <- if (is.null(V(graph)$name)) {
        1:n_vertices
      } else {
        as.numeric(V(graph)$name)
      }
      
      # Get bus status
      if (exists("bus_info") && nrow(bus_info) > 0) {
        bus_status <- bus_info %>%
          mutate(is_active = bus_i %in% active_buses)
        
        total_load <- sum(bus_status$load_mw, na.rm = TRUE)
        total_gen <- sum(bus_status$total_gen, na.rm = TRUE)
        load_served <- sum(bus_status$load_mw[bus_status$is_active], na.rm = TRUE)
        gen_served <- sum(bus_status$total_gen[bus_status$is_active], na.rm = TRUE)
        
        load_served_pct <- if (total_load > 0) 100 * load_served / total_load else 0
        gen_served_pct <- if (total_gen > 0) 100 * gen_served / total_gen else 0
      } else {
        load_served_pct <- 0
        gen_served_pct <- 0
      }
      
      return(list(
        largest_component = largest_component,
        n_components = n_components,
        load_served_pct = load_served_pct,
        gen_served_pct = gen_served_pct
      ))
      
    }, seed = TRUE)
    
    # PARALLEL: Network topology metrics
    network_metrics <- future({
      
      avg_node_strength <- 0
      algebraic_connectivity <- 0
      
      if (n_edges > 0 && n_vertices > 1) {
        # Node strength calculation
        adj_matrix <- as_adjacency_matrix(graph, attr = "Yreal", sparse = FALSE)
        W <- as.matrix(adj_matrix)
        strength <- rowSums(abs(W))
        avg_node_strength <- mean(strength, na.rm = TRUE)
        
        # Algebraic connectivity
        if (n_vertices > 2) {
          L <- diag(rowSums(abs(W))) - W
          eigenvals <- eigen(L, symmetric = TRUE, only.values = TRUE)$values
          eigenvals <- sort(eigenvals)
          algebraic_connectivity <- if (length(eigenvals) > 1) eigenvals[2] else 0
        }
      }
      
      return(list(
        avg_node_strength = avg_node_strength,
        algebraic_connectivity = algebraic_connectivity
      ))
      
    }, seed = TRUE)
    
    # Wait for parallel computations
    pm <- value(parallel_metrics)
    nm <- value(network_metrics)
    
    # Cascade metrics
    fire_count <- length(fire_affected)
    cascade_count <- length(deenergized)
    total_lost_count <- length(buses_lost) # Use a different name to avoid conflict
    cascade_ratio <- if (fire_count > 0) cascade_count / fire_count else 0
    
    # CORRECTED: Return a clean, consistent dataframe
    return(data.frame(
      step = step_num,
      vertices_remaining = n_vertices,
      edges_remaining = n_edges,
      largest_component = pm$largest_component,
      n_components = pm$n_components,
      load_served_pct = pm$load_served_pct,
      gen_served_pct = pm$gen_served_pct,
      algebraic_connectivity = nm$algebraic_connectivity,
      avg_node_strength = nm$avg_node_strength,
      buses_lost_count = total_lost_count,
      fire_affected = fire_count,
      deenergized = cascade_count,
      cascade_ratio = cascade_ratio
    ))
    
  }, error = function(e) {
    message("Error in parallel metrics calculation: ", e$message)
    return(create_default_metrics(step_num, buses_lost, fire_affected, deenergized))
  })
}


# Helper function for default metrics
create_default_metrics <- function(step_num, buses_lost, fire_affected, deenergized) {
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
    cascade_ratio = 0
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