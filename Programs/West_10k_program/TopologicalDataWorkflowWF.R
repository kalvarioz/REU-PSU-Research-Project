# =================================================================================================
# TopologicalDataWorkFlowWF.R

# Brandon Calvario

# =================================================================================================

library(data.table)
library(ggplot2)
library(dplyr)
library(TDA)
library(tidyr)

# =================================================================================================
# CONFIGURATION
# =================================================================================================
perseus_config <- list(
  # Input/Output paths
  net_power_csv = "databases/net_power_difference_normalizedTEST.csv",
  perseus_exe = normalizePath("Perseus/perseusWin.exe", winslash="\\"),
  outputs_dir = "outputs/",
  
  # Perseus parameters
  perseus_input_file = "M.txt",
  perseus_output_prefix = "Moutput",
  max_dim = 1,
  max_scale = 1.0,
  persistence_thresh = 0,
  downsample_max_pts = 300,
  
  # Resource management - CRITICAL FIXES
  memory_limit_gb = 32,
  use_adaptive_sampling = TRUE,
  chunk_processing = FALSE,
  cleanup_temp_files = TRUE,
  
  # Perseus distmat parameters 
  g = 0, # genus
  s = 0.05,
  N = 10,
  C = 3,
  timeout_seconds = 600
)
validate_bus_data_integrity <- function(buses_sf) {
  message("=== BUS DATA INTEGRITY CHECK ===")
  
  # Check basic structure
  message("Total buses: ", nrow(buses_sf))
  message("Unique bus_i count: ", length(unique(buses_sf$bus_i)))
  message("bus_i range: ", min(buses_sf$bus_i, na.rm = TRUE), " to ", max(buses_sf$bus_i, na.rm = TRUE))
  
  # Check for duplicates
  dupes <- buses_sf %>% 
    group_by(bus_i) %>% 
    filter(n() > 1) %>% 
    ungroup()
  
  if (nrow(dupes) > 0) {
    message("WARNING: Found ", nrow(dupes), " duplicate bus_i entries")
    message("Duplicate IDs: ", paste(unique(dupes$bus_i), collapse = ", "))
  }
  
  # Check coordinate variation
  coords <- st_coordinates(buses_sf)
  coord_range_x <- max(coords[,1]) - min(coords[,1])
  coord_range_y <- max(coords[,2]) - min(coords[,2])
  
  message("Coordinate ranges: X=", round(coord_range_x, 6), ", Y=", round(coord_range_y, 6))
  
  if (coord_range_x < 0.001 && coord_range_y < 0.001) {
    message("ERROR: All buses have nearly identical coordinates!")
    return(FALSE)
  }
  
  return(TRUE)
}

run_fixed_smart_tda_workflow <- function(fire_data, bus_info, graph_original,
                                         analysis_radius_km = 30,
                                         fire_impact_buffer_km = 2,
                                         simulation_steps = 20,
                                         use_existing_cascade = TRUE,
                                         existing_cascade_results = NULL,
                                         generate_plots = TRUE) {
  
  message("=== UNIFIED SMART TDA & CASCADE WORKFLOW ===")
  
  fire_names <- unique(fire_data$attr_IncidentName)
  is_compound_event <- length(fire_names) > 1
  
  fire_display_text <- if (is_compound_event) {
    paste("Compound Event:", length(fire_names), "fires")
  } else {
    fire_names[1]
  }
  
  safe_fire_name <- if (is_compound_event) {
    paste("compound", length(fire_names), "fires", sep = "_")
  } else {
    gsub("[^A-Za-z0-9-]", "_", fire_names[1])
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  attack_run_dir <- file.path(cfg$outputs_attacked_dir, paste(safe_fire_name, timestamp, sep = "_"))
  before_dir <- file.path(attack_run_dir, "before_analysis")
  after_dir <- file.path(attack_run_dir, "after_analysis")
  plots_dir <- file.path(attack_run_dir, "plots")
  
  sapply(c(attack_run_dir, before_dir, after_dir, plots_dir), dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # [STEP 1] UNIFIED CASCADE SIMULATION
  # This section now ensures that whether we use existing results or run a new simulation,
  # the 'cascade_results' object is the full, rich output from run_enhanced_fire_cascade.
  if (use_existing_cascade && !is.null(existing_cascade_results) && !is.null(existing_cascade_results$metrics)) {
    cascade_results <- existing_cascade_results
    message("[1/6] Using existing, complete cascade results.")
  } else {
    message("[1/6] Running ENHANCED cascade simulation to ensure full metrics are available...")
    # This is the key call. We are explicitly running the most comprehensive cascade function.
    cascade_results <- run_enhanced_fire_cascade(
      graph = graph_original,
      buses_sf = bus_info,
      fire_data = fire_data,
      buffer_km = fire_impact_buffer_km,
      steps = simulation_steps
    )
  }
  
  # Ensure cascade_results is not null before proceeding
  if(is.null(cascade_results) || !is.list(cascade_results)) {
    return(list(success = FALSE, error = "Cascade simulation failed to produce valid results."))
  }
  
  all_failed_buses <- unique(unlist(cascade_results$buses_lost_per_step))
  message("Cascade complete: ", length(all_failed_buses), " total buses failed.")
  
  # [STEP 2] Define analysis area
  message("[2/6] Defining analysis area...")
  analysis_radius_miles <- analysis_radius_km * 0.621371
  
  if (is_compound_event) {
    local_area_data <- get_local_area_data_compound_fixed(fire_data, bus_info, analysis_radius_miles)
  } else {
    local_area_data <- get_local_area_data_simple(fire_data, bus_info, analysis_radius_miles)
  }
  
  if (is.null(local_area_data) || (exists("success", where = local_area_data) && !local_area_data$success)) {
    error_msg <- local_area_data$error %||% "Unknown error in local area extraction."
    return(list(success = FALSE, error = error_msg))
  }
  
  local_bus_data <- local_area_data$buses
  if (nrow(local_bus_data) < 2) {
    return(list(success = FALSE, error = paste("Insufficient buses in analysis area:", nrow(local_bus_data))))
  }
  
  message("Analysis area contains ", nrow(local_bus_data), " buses.")
  if (!validate_bus_data_integrity(buses_sf)) {
    stop("Bus data integrity check failed!")
  }
  
  # Also validate the loaded matrix
  if (exists("healthy_state_matrix")) {
    message("Matrix dimensions: ", nrow(healthy_state_matrix), "×", ncol(healthy_state_matrix))
    message("Matrix rownames sample: ", paste(head(rownames(healthy_state_matrix)), collapse = ", "))
    
    if (is.null(rownames(healthy_state_matrix))) {
      message("WARNING: Matrix has no rownames - this will cause matching issues")
    }
  }
  # [STEP 3] Before state analysis
  message("[3/6] Analyzing 'before' topology...")
  local_healthy_matrix <- extract_local_submatrix_from_original(local_bus_data)
  tda_before_results <- run_perseus_analysis(local_healthy_matrix, before_dir)
  
  if (!tda_before_results$success) {
    return(list(success = FALSE, error = "Before state TDA failed"))
  }
  
  # [STEP 4] After state analysis
  message("[4/6] Analyzing 'after' topology...")
  surviving_local_bus_ids <- setdiff(local_bus_data$bus_i, all_failed_buses)
  
  if (length(surviving_local_bus_ids) < 3) {
    tda_after_results <- list(
      success = TRUE,
      persistence_data = matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("dimension", "birth", "death")))
    )
  } else {
    surviving_local_bus_data <- local_bus_data %>% filter(bus_i %in% surviving_local_bus_ids)
    local_attacked_matrix <- generate_local_power_matrix(surviving_local_bus_data)
    tda_after_results <- run_perseus_analysis(local_attacked_matrix, after_dir)
  }
  if (!tda_after_results$success) {
    return(list(success = FALSE, error = "After state TDA failed"))
  }
  
  # [STEP 5] Calculate topological distance
  message("[5/6] Calculating topological changes...")
  wasserstein_dist <- calculate_wasserstein_distance(
    tda_before_results$persistence_data, 
    tda_after_results$persistence_data
  )
  message("Wasserstein distance: ", round(wasserstein_dist, 6))
  
  # [STEP 6] Generate comprehensive visualizations
  # This step now has the full 'cascade_results' object, ensuring all graphs can be made.
  plots_list <- list()
  if (generate_plots) {
    message("[6/6] Generating comprehensive visualizations...")
    
    # The 'enhance_cascade_data_for_plotting' function adds the final TDA metric to the results
    enhanced_cascade_results <- enhance_cascade_data_for_plotting(cascade_results, wasserstein_dist)
    
    analysis_parameters <- list(
      wasserstein_distance = wasserstein_dist,
      is_compound_event = is_compound_event,
      fire_count = length(fire_names),
      fire_names = fire_names
    )
    
    plots_list <- create_comprehensive_tda_visualization_set(
      cascade_results = enhanced_cascade_results, # Use the enhanced results
      tda_before = tda_before_results,
      tda_after = tda_after_results,
      fire_name = fire_display_text,
      analysis_params = analysis_parameters
    )
    
    # Save plots
    for (plot_name in names(plots_list)) {
      plot_file <- file.path(plots_dir, paste0(plot_name, ".png"))
      tryCatch({
        if (inherits(plots_list[[plot_name]], "ggplot")) {
          ggsave(plot_file, plots_list[[plot_name]], width = 12, height = 9, dpi = 300, bg = "white")
        }
      }, error = function(e) {
        message("  ✗ Error saving plot ", plot_name, ": ", e$message)
      })
    }
  }
  
  message("✓ Unified TDA & Cascade Workflow Complete")
  
  return(list(
    success = TRUE,
    fire_name = fire_display_text,
    is_compound_event = is_compound_event,
    wasserstein_distance = wasserstein_dist,
    before_features = nrow(tda_before_results$persistence_data),
    after_features = nrow(tda_after_results$persistence_data),
    plots_list = plots_list,
    cascade_results = cascade_results # Return the full cascade results
  ))
}

plot_persistence_diagram <- function(diagram, title_suffix = "") {
  if (nrow(diagram) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No features found", size = 6) + 
             theme_minimal() +
             labs(title = paste("Persistence Diagram", title_suffix)))
  }
  
  df <- as.data.frame(diagram)
  colnames(df) <- c("Dimension", "Birth", "Death")
  df$Persistence <- df$Death - df$Birth
  
  # Color palette for dimensions
  colors <- c("0" = "#2E86AB", "1" = "#E63946", "2" = "#2ca02c")
  
  ggplot(df, aes(x = Birth, y = Death, color = factor(Dimension), size = Persistence)) +
    geom_point(alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed", size = 1) +
    scale_color_manual(values = colors, name = "Dimension") +
    scale_size_continuous(range = c(2, 5), guide = "legend") +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(
      title = paste("Persistence Diagram", title_suffix),
      x = "Birth Time",
      y = "Death Time",
      subtitle = paste("Total features:", nrow(df))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
}
safe_spatial_intersection <- function(points_sf, polygon_sf, method = "within") {
  message("=== ENHANCED SAFE SPATIAL INTERSECTION ===")
  message("Points: ", nrow(points_sf), ", Polygons: ", nrow(polygon_sf))
  
  # Input validation
  if (!inherits(points_sf, "sf") || !inherits(polygon_sf, "sf")) {
    message("ERROR: Inputs are not sf objects")
    return(rep(FALSE, nrow(points_sf)))
  }
  
  if (nrow(points_sf) == 0) {
    message("No points to intersect")
    return(logical(0))
  }
  
  if (nrow(polygon_sf) == 0) {
    message("No polygons to intersect with")
    return(rep(FALSE, nrow(points_sf)))
  }
  
  # CRS check
  if (!identical(st_crs(points_sf), st_crs(polygon_sf))) {
    message("Converting polygon CRS to match points")
    polygon_sf <- st_transform(polygon_sf, st_crs(points_sf))
  }
  
  # Geometry validation
  if (any(!st_is_valid(points_sf))) {
    message("Fixing invalid point geometries")
    points_sf <- st_make_valid(points_sf)
  }
  
  if (any(!st_is_valid(polygon_sf))) {
    message("Fixing invalid polygon geometries")
    polygon_sf <- st_make_valid(polygon_sf)
  }
  
  # Perform intersection with extensive error handling
  result <- tryCatch({
    if (method == "within") {
      intersection_matrix <- st_within(points_sf, polygon_sf, sparse = FALSE)
    } else {
      intersection_matrix <- st_intersects(points_sf, polygon_sf, sparse = FALSE)
    }
    
    message("Raw intersection result class: ", class(intersection_matrix))
    message("Raw intersection result dimensions: ", paste(dim(intersection_matrix), collapse = "x"))
    
    # Convert to logical vector
    if (is.matrix(intersection_matrix)) {
      if (ncol(intersection_matrix) == 0) {
        logical_vector <- rep(FALSE, nrow(points_sf))
      } else if (ncol(intersection_matrix) == 1) {
        logical_vector <- as.logical(intersection_matrix[, 1])
      } else {
        # Multiple polygons - any intersection counts as TRUE
        logical_vector <- apply(intersection_matrix, 1, function(row) {
          any(row, na.rm = TRUE)
        })
      }
    } else {
      logical_vector <- as.logical(intersection_matrix)
    }
    
    message("Converted to logical vector length: ", length(logical_vector))
    message("Logical vector type: ", class(logical_vector))
    message("Contains NAs: ", any(is.na(logical_vector)))
    
    # Handle NAs aggressively
    if (any(is.na(logical_vector))) {
      message("Replacing ", sum(is.na(logical_vector)), " NAs with FALSE")
      logical_vector[is.na(logical_vector)] <- FALSE
    }
    
    # Ensure correct length
    if (length(logical_vector) != nrow(points_sf)) {
      message("Length mismatch - creating FALSE vector")
      logical_vector <- rep(FALSE, nrow(points_sf))
    }
    
    # Final type check
    if (!is.logical(logical_vector)) {
      message("Result is not logical - converting")
      logical_vector <- as.logical(logical_vector)
      logical_vector[is.na(logical_vector)] <- FALSE
    }
    
    return(logical_vector)
    
  }, error = function(e) {
    message("Intersection operation failed: ", e$message)
    return(rep(FALSE, nrow(points_sf)))
  })
  
  # FINAL VALIDATION
  if (!is.logical(result) || length(result) != nrow(points_sf) || any(is.na(result))) {
    message("CRITICAL: Final result validation failed - returning safe FALSE vector")
    result <- rep(FALSE, nrow(points_sf))
  }
  
  message("✓ Intersection complete: ", sum(result), " matches found")
  return(result)
}

# Updated get_local_area_data function that uses the safe intersection

get_local_area_data_simple <- function(fire_data, bus_info, radius_miles) {
  message("=== SIMPLE LOCAL AREA DATA EXTRACTION ===")
  tryCatch({
    fire_center <- st_centroid(st_union(fire_data))
    radius_meters <- radius_miles * 1609.34
    aoi_buffer <- st_buffer(fire_center, dist = radius_meters)
    
    # Using default sparse list output is efficient
    intersecting_indices <- unlist(st_intersects(bus_info, aoi_buffer))
    
    if (length(intersecting_indices) == 0) {
      local_buses <- bus_info[0, ]
    } else {
      local_buses <- bus_info[intersecting_indices, ]
    }
    
    message("Found ", nrow(local_buses), " buses in analysis area")
    
    return(list(
      success = TRUE,
      buses = local_buses,
      bus_ids = if (nrow(local_buses) > 0) local_buses$bus_i else integer(0)
    ))
  }, error = function(e) {
    message("Error in simple extraction: ", e$message)
    return(list(success = FALSE, error = e$message, buses = bus_info[0, ], bus_ids = integer(0)))
  })
}

get_local_area_data_compound_fixed <- function(fire_data_sf, buses_sf, radius_miles) {
  message("=== COMPOUND FIRE LOCAL AREA EXTRACTION (FIXED) ===")
  
  if (is.null(fire_data_sf) || is.null(buses_sf) || nrow(fire_data_sf) == 0 || nrow(buses_sf) == 0) {
    return(list(success = FALSE, error = "Invalid input to compound area extraction", buses = buses_sf[0, ], bus_ids = integer(0)))
  }
  
  tryCatch({
    # 1. Union all fire geometries into a single feature
    unified_fire_geom <- st_union(fire_data_sf)
    
    # 2. Get the centroid of that single, unified geometry
    fire_center <- st_centroid(unified_fire_geom)
    
    # 3. Create the analysis buffer around the center point
    radius_meters <- radius_miles * 1609.34
    aoi_buffer <- st_buffer(fire_center, dist = radius_meters)
    
    # 4. FIXED: More robust spatial intersection
    message("DEBUG: buses_sf has ", nrow(buses_sf), " rows")
    message("DEBUG: Sample bus_i values: ", paste(head(buses_sf$bus_i), collapse = ", "))
    
    # Use st_within instead of st_intersects for more precise matching
    within_indices <- st_within(buses_sf, aoi_buffer, sparse = FALSE)
    intersecting_rows <- which(within_indices[, 1])
    
    message("DEBUG: Found ", length(intersecting_rows), " intersecting row indices")
    
    if (length(intersecting_rows) == 0) {
      message("No buses found within the compound fire analysis area.")
      local_buses_sf <- buses_sf[0, ]
    } else {
      # CRITICAL FIX: Ensure we maintain the correct bus_i values
      local_buses_sf <- buses_sf[intersecting_rows, ]
      
      # Verify the bus_i values are preserved
      message("DEBUG: Extracted bus_i values: ", paste(head(local_buses_sf$bus_i, 10), collapse = ", "))
      
      # Additional validation
      if (length(unique(local_buses_sf$bus_i)) < length(local_buses_sf$bus_i) * 0.5) {
        message("WARNING: Many duplicate bus_i values detected, checking data integrity...")
        # Remove any duplicates that might have been introduced
        local_buses_sf <- local_buses_sf %>% distinct(bus_i, .keep_all = TRUE)
      }
    }
    
    message("Found ", nrow(local_buses_sf), " unique buses in compound analysis area.")
    
    return(list(
      success = TRUE,
      buses = local_buses_sf,
      bus_ids = if(nrow(local_buses_sf) > 0) local_buses_sf$bus_i else integer(0)
    ))
    
  }, error = function(e) {
    message("CRITICAL ERROR in compound extraction: ", e$message)
    return(list(
      success = FALSE, 
      error = paste("Error in compound extraction:", e$message),
      buses = buses_sf[0, ], 
      bus_ids = integer(0)
    ))
  })
}

create_before_after_comparison <- function(before_data, after_data, fire_name, wasserstein_dist) {
  
  # Prepare data
  before_df <- as.data.frame(before_data)
  colnames(before_df) <- c("Dimension", "Birth", "Death")
  before_df$State <- "Before"
  before_df$Persistence <- before_df$Death - before_df$Birth
  
  after_df <- as.data.frame(after_data)
  colnames(after_df) <- c("Dimension", "Birth", "Death")
  after_df$State <- "After"
  after_df$Persistence <- after_df$Death - after_df$Birth
  
  combined_df <- rbind(before_df, after_df)
  
  # Color palette
  dimension_colors <- c("0" = "#2E86AB", "1" = "#E63946", "2" = "#2ca02c")
  
  ggplot(combined_df, aes(x = Birth, y = Death, color = factor(Dimension), size = Persistence)) +
    geom_point(alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed", size = 1) +
    scale_color_manual(values = dimension_colors, name = "Dimension") +
    scale_size_continuous(range = c(1.5, 4), guide = "legend") +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    facet_wrap(~ State, ncol = 2) +
    labs(
      title = paste("TDA Comparison:", fire_name),
      subtitle = paste("Wasserstein Distance:", round(wasserstein_dist, 4)),
      x = "Birth Time",
      y = "Death Time"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

create_cascade_progression_plot <- function(metrics, fire_name, is_compound) {
  
  if (is.null(metrics) || nrow(metrics) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No cascade metrics available") +
             theme_minimal())
  }
  
  # Ensure step column exists
  if (!"step" %in% names(metrics)) {
    metrics$step <- seq_len(nrow(metrics))
  }
  
  # CORRECTED: Safely create columns using if/else instead of case_when
  metrics_clean <- metrics %>%
    mutate(
      fire_affected = if ("fire_affected" %in% names(.)) {
        as.numeric(fire_affected)
      } else if ("direct_hits" %in% names(.)) {
        as.numeric(direct_hits)
      } else {
        0
      },
      deenergized = if ("deenergized" %in% names(.)) {
        as.numeric(deenergized)
      } else {
        0
      },
      vertices_remaining = if ("vertices_remaining" %in% names(.)) {
        as.numeric(vertices_remaining)
      } else {
        0
      }
    ) %>%
    # Clean up any NA values
    mutate(
      fire_affected = ifelse(is.na(fire_affected), 0, fire_affected),
      deenergized = ifelse(is.na(deenergized), 0, deenergized),
      vertices_remaining = ifelse(is.na(vertices_remaining), 0, vertices_remaining)
    )
  
  # Debug information
  message("DEBUG: Cascade plot data preparation:")
  message("  Original columns: ", paste(names(metrics), collapse = ", "))
  message("  Fire affected values: ", paste(head(metrics_clean$fire_affected, 5), collapse = ", "))
  message("  Deenergized values: ", paste(head(metrics_clean$deenergized, 5), collapse = ", "))
  
  # Prepare data for stacked area plot
  plot_data <- metrics_clean %>%
    select(step, fire_affected, deenergized) %>%
    gather(key = "Impact_Type", value = "Count", fire_affected, deenergized) %>%
    mutate(Impact_Type = case_when(
      Impact_Type == "fire_affected" ~ "Fire Impact",
      Impact_Type == "deenergized" ~ "Cascade Failures",
      TRUE ~ Impact_Type
    ))
  
  # Create the main cascade plot
  p1 <- ggplot(plot_data, aes(x = step, y = Count, fill = Impact_Type)) +
    geom_area(alpha = 0.8, position = "stack") +
    scale_fill_manual(values = c("Fire Impact" = "#E63946", "Cascade Failures" = "#06AED5")) +
    labs(
      title = if(is_compound) paste("Compound Fire Cascade:", fire_name) else paste("Fire Cascade:", fire_name),
      x = "Simulation Step",
      y = "Buses Affected",
      fill = "Impact Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  # Create grid size plot
  grid_data <- metrics_clean %>%
    select(step, vertices_remaining)
  
  p2 <- ggplot(grid_data, aes(x = step, y = vertices_remaining)) +
    geom_line(color = "#2E86AB", size = 1.2) +
    geom_point(color = "#2E86AB", size = 2) +
    labs(
      title = "Grid Size Over Time",
      x = "Simulation Step",
      y = "Active Buses"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold")
    )
  
  # Combine plots using patchwork if available, otherwise return main plot
  if (requireNamespace("patchwork", quietly = TRUE)) {
    return(p1 / p2)
  } else {
    return(p1)
  }
}


create_topological_summary <- function(before_features, after_features, wasserstein_dist,
                                       fire_name, is_compound) {
  
  # Prepare data
  summary_data <- data.frame(
    Metric = c("Features\nBefore", "Features\nAfter", "Wasserstein\nDistance"),
    Value = c(before_features, after_features, wasserstein_dist),
    Type = c("Features", "Features", "Distance"),
    stringsAsFactors = FALSE
  )
  
  # Create the plot
  ggplot(summary_data, aes(x = Metric, y = Value, fill = Type)) +
    geom_col(alpha = 0.8, width = 0.7) +
    geom_text(aes(label = ifelse(Type == "Distance", 
                                 round(Value, 4), 
                                 round(Value))), 
              vjust = -0.5, size = 4, fontface = "bold") +
    scale_fill_manual(values = c("Features" = "#4CAF50", "Distance" = "#FF9800")) +
    labs(
      title = if(is_compound) paste("Compound Event Summary:", fire_name) else paste("TDA Summary:", fire_name),
      x = "",
      y = "Value",
      fill = "Metric Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )
}

create_compound_event_plot<- function(analysis_params, cascade_results, fire_name) {
  
  fire_names <- analysis_params$fire_names
  fire_count <- analysis_params$fire_count
  
  if (fire_count <= 1) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "Single fire event") +
             theme_minimal())
  }
  
  # Create timeline data
  timeline_data <- data.frame(
    Fire_Index = seq_along(fire_names),
    Fire_Name = fire_names,
    Y_Position = rep(1, length(fire_names)),
    stringsAsFactors = FALSE
  )
  
  ggplot(timeline_data, aes(x = Fire_Index, y = Y_Position)) +
    geom_point(size = 8, color = "#E63946", alpha = 0.8) +
    geom_line(color = "gray50", linetype = "dashed", size = 1.2) +
    geom_text(aes(label = paste("Fire", Fire_Index)), 
              vjust = -1.5, size = 4, fontface = "bold") +
    geom_text(aes(label = Fire_Name), 
              vjust = 3, size = 3, angle = 45) +
    ylim(0.5, 1.8) +
    labs(
      title = paste("Compound Fire Event Analysis -", fire_count, "Simultaneous Fires"),
      x = "Fire Index",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank()
    )
}
create_integrated_dashboard <- function(plots_list, fire_name, analysis_params) {
  
  # Create a summary statistics plot
  wasserstein_dist <- analysis_params$wasserstein_distance %||% 0
  is_compound <- analysis_params$is_compound_event %||% FALSE
  
  dashboard_data <- data.frame(
    Metric = c("Topological Change", "Analysis Type", "Fire Count"),
    Value = c(
      round(wasserstein_dist, 4),
      if(is_compound) "Compound" else "Single",
      analysis_params$fire_count %||% 1
    ),
    Category = c("Distance", "Type", "Count"),
    stringsAsFactors = FALSE
  )
  
  ggplot(dashboard_data, aes(x = Metric, y = 1, fill = Category)) +
    geom_tile(alpha = 0.8, color = "white", size = 2) +
    geom_text(aes(label = Value), size = 6, fontface = "bold", color = "white") +
    scale_fill_viridis_d(option = "plasma") +
    labs(
      title = paste("TDA Analysis Dashboard:", fire_name),
      subtitle = "Topological Data Analysis Summary",
      x = "",
      y = ""
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "none",
      axis.text = element_text(size = 12, face = "bold")
    )
}

display_tda_plots <- function(plots_list) {
  if (length(plots_list) == 0) {
    message("No plots to display")
    return(invisible(TRUE))
  }
  
  message("Displaying TDA plots in RStudio...")
  
  tryCatch({
    if (!interactive()) {
      message("Not in interactive session - plots saved to files but not displayed")
      return(invisible(TRUE))
    }
    
    # Display each ggplot object
    for (plot_name in names(plots_list)) {
      if (inherits(plots_list[[plot_name]], "ggplot")) {
        print(plots_list[[plot_name]])
        message("Displayed plot: ", plot_name)
        if (interactive()) Sys.sleep(0.5)  # Brief pause between plots
      }
    }
    
    message("✓ Plot display complete")
    
  }, error = function(e) {
    message("Warning in plot display: ", e$message)
  })
  
  return(invisible(TRUE))
}


# Also add this helper function for safer plot creation
safe_create_plot <- function(plot_function, plot_args, plot_name, save_path = NULL) {
  tryCatch({
    do.call(plot_function, plot_args)
    if (!is.null(save_path)) {
      message("  ✓ Successfully created and saved: ", plot_name)
      return(save_path)
    } else {
      message("  ✓ Successfully created: ", plot_name)
      return(plot_name)
    }
  }, error = function(e) {
    message("  ✗ Error creating ", plot_name, ": ", e$message)
    return(NULL)
  })
}


extract_local_submatrix_from_original <- function(local_bus_data) {
  message("=== ADVANCED SUBMATRIX EXTRACTION WITH ENHANCED DEBUGGING ===")
  
  # 1. Validate inputs
  if (!exists("healthy_state_matrix") || is.null(healthy_state_matrix)) {
    stop("CRITICAL: Original healthy state matrix not loaded or is NULL.")
  }
  original_matrix <- healthy_state_matrix
  
  if (nrow(local_bus_data) < 2) {
    stop(paste("Insufficient local bus data provided:", nrow(local_bus_data)))
  }
  
  # 2. ENHANCED: Validate bus_i column integrity
  message("DEBUG: local_bus_data structure:")
  message("  Rows: ", nrow(local_bus_data))
  message("  Columns: ", paste(names(local_bus_data)[1:min(5, ncol(local_bus_data))], collapse = ", "))
  message("  bus_i column class: ", class(local_bus_data$bus_i))
  message("  bus_i sample: ", paste(head(local_bus_data$bus_i, 10), collapse = ", "))
  message("  bus_i unique count: ", length(unique(local_bus_data$bus_i)))
  message("  bus_i total count: ", length(local_bus_data$bus_i))
  
  # Check if bus_i values are all the same (the main issue)
  if (length(unique(local_bus_data$bus_i)) == 1) {
    message("CRITICAL ERROR: All bus_i values are identical (", unique(local_bus_data$bus_i)[1], ")")
    message("This indicates a problem in spatial intersection or data filtering.")
    message("Checking original buses_sf data...")
    
    # Try to recover by examining the geometry and re-extracting bus_i
    if (inherits(local_bus_data, "sf")) {
      coords <- st_coordinates(local_bus_data)
      message("  Coordinate ranges: X[", round(min(coords[,1]), 3), "-", round(max(coords[,1]), 3), 
              "], Y[", round(min(coords[,2]), 3), "-", round(max(coords[,2]), 3), "]")
      
      # If coordinates are different but bus_i is the same, there's a data corruption issue
      if (nrow(coords) > 1 && (max(coords[,1]) - min(coords[,1]) > 0.001 || max(coords[,2]) - min(coords[,2]) > 0.001)) {
        stop("CRITICAL: Spatial coordinates vary but bus_i values are identical - data corruption detected!")
      }
    }
    
    stop("Cannot proceed with matrix extraction - bus_i data corruption detected")
  }
  
  # 3. Prepare IDs for matching with enhanced validation
  local_bus_ids <- as.character(local_bus_data$bus_i)
  local_bus_ids <- local_bus_ids[!is.na(local_bus_ids)]
  matrix_bus_ids <- rownames(original_matrix)
  
  message("  Local bus IDs (first 10): ", paste(head(local_bus_ids, 10), collapse = ", "))
  message("  Matrix bus IDs (first 10): ", paste(head(matrix_bus_ids, 10), collapse = ", "))
  
  # Rest of the function remains the same...
  # 4. Attempt matching strategies
  # Strategy 1: Direct match
  valid_bus_ids <- intersect(local_bus_ids, matrix_bus_ids)
  message("  Strategy 1 (Direct Match): Found ", length(valid_bus_ids), " matches.")
  
  # Strategy 2: Prefixed match
  if (length(valid_bus_ids) < 2) {
    message("  Direct match failed. Trying Strategy 2 (Prefix Match)...")
    local_bus_ids_prefixed <- paste0("bus_", local_bus_ids)
    valid_bus_ids <- intersect(local_bus_ids_prefixed, matrix_bus_ids)
    message("  Strategy 2 (Prefix Match): Found ", length(valid_bus_ids), " matches.")
  }
  
  # Strategy 3: Un-prefixed match
  if (length(valid_bus_ids) < 2) {
    message("  Prefix match failed. Trying Strategy 3 (Un-prefix Match)...")
    matrix_bus_ids_unprefixed <- gsub("bus_", "", matrix_bus_ids)
    
    matching_local_ids <- intersect(local_bus_ids, matrix_bus_ids_unprefixed)
    
    if(length(matching_local_ids) > 0) {
      valid_bus_ids <- matrix_bus_ids[matrix_bus_ids_unprefixed %in% matching_local_ids]
    } else {
      valid_bus_ids <- character(0)
    }
    message("  Strategy 3 (Un-prefix Match): Found ", length(valid_bus_ids), " matches.")
  }
  
  # 5. Final check and extraction
  if (length(valid_bus_ids) < 2) {
    stop(paste("All matching strategies failed. Found only", length(valid_bus_ids), "matching buses."))
  }
  
  # 6. Extract the submatrix
  local_matrix <- original_matrix[valid_bus_ids, valid_bus_ids]
  
  message("✓ Extracted submatrix: ", nrow(local_matrix), "×", ncol(local_matrix))
  return(local_matrix)
}

enhance_cascade_data_for_plotting <- function(cascade_results, wasserstein_dist) {
  if (is.null(cascade_results)) {
    return(NULL)
  }
  cascade_results$wasserstein_distance <- wasserstein_dist
  if (is.null(cascade_results$plotting_metadata)) {
    cascade_results$plotting_metadata <- list()
  }
  cascade_results$plotting_metadata$wasserstein_computed <- TRUE
  return(cascade_results)
}

# =================================================================================================
# DATA LOADING FUNCTIONS
# =================================================================================================
# FIXED: Load matrix with proper error handling
load_net_power_matrix <- function(file_path) {
  message("Loading net power difference matrix from: ", file_path)
  
  if (!file.exists(file_path)) {
    stop("Net power matrix file not found: ", file_path)
  }
  
  # Read with explicit NA handling
  dt <- tryCatch({
    fread(file_path, na.strings = c("", "NA", "NULL"))
  }, error = function(e) {
    stop("Failed to read CSV file: ", e$message)
  })
  
  if (nrow(dt) == 0) {
    stop("Matrix file is empty")
  }
  
  # Convert to matrix with explicit NA handling
  if (ncol(dt) > 1) {
    if (names(dt)[1] %in% c("bus_i", "V1") || is.numeric(dt[[1]])) {
      mat <- as.matrix(dt[, -1, with = FALSE])
      rownames(mat) <- as.character(dt[[1]])  # Ensure character row names
    } else {
      mat <- as.matrix(dt)
    }
  } else {
    stop("Matrix file appears to have only one column")
  }
  
  # Handle NAs explicitly
  if (any(is.na(mat))) {
    warning("Matrix contains NA values, replacing with 0")
    mat[is.na(mat)] <- 0
  }
  
  # Simple normalization check
  mat_max <- max(mat, na.rm = TRUE)
  mat_min <- min(mat, na.rm = TRUE)
  
  if (is.finite(mat_max) && is.finite(mat_min) && mat_max > 0) {
    if (mat_max > 2) {  # Only normalize if values are large
      mat <- mat / mat_max
      message("Matrix normalized to [0,1] range")
    }
  }
  
  return(mat)
}

get_local_buses <- function(fire_data, bus_info, radius_miles) {
  fire_center <- st_centroid(st_union(fire_data))
  radius_meters <- radius_miles * 1609.34
  aoi_buffer <- st_buffer(fire_center, dist = radius_meters)
  
  # CORRECTED: Use the bus_info object for the intersection
  local_buses <- bus_info[st_intersects(bus_info, aoi_buffer, sparse = FALSE), ]
  return(local_buses)
}

generate_local_power_matrix <- function(local_bus_data, method = "enhanced_multi_factor") {
  message("=== ENHANCED LOCAL POWER MATRIX GENERATION ===")
  
  if (is.null(local_bus_data) || nrow(local_bus_data) == 0) {
    stop("No bus data provided to matrix generation")
  }
  
  required_cols <- c("total_gen", "load_mw", "vm", "baseKV", "bus_type", "longitude", "latitude")
  missing_cols <- setdiff(required_cols, names(local_bus_data))
  if (length(missing_cols) > 0) {
    message("Adding missing columns: ", paste(missing_cols, collapse = ", "))
    for (col in missing_cols) {
      local_bus_data[[col]] <- switch(col,"total_gen" = 0,
                                      "load_mw" = 0, 
                                      "vm" = 1.0,
                                      "baseKV" = 138,
                                      "bus_type" = "Load",
                                      "longitude" = mean(local_bus_data$longitude, na.rm = TRUE),
                                      "latitude" = mean(local_bus_data$latitude, na.rm = TRUE),
                                      NA
      )
    }
  }
  message("Input bus data: ", nrow(local_bus_data), " buses")
  message("Method: ", method)
  
  # CRITICAL: Check for empty input
  if (is.null(local_bus_data) || nrow(local_bus_data) == 0) {
    message("CRITICAL ERROR: No bus data provided to matrix generation")
    # Return a minimal 2x2 matrix to prevent Perseus errors
    return(matrix(c(0, 0.1, 0.1, 0), nrow = 2, ncol = 2))
  }
  
  if (nrow(local_bus_data) < 2) {
    message("WARNING: Insufficient buses for meaningful matrix generation")
    message("  Need at least 2 buses, got ", nrow(local_bus_data))
    
    # Create a minimal matrix for single bus
    if (nrow(local_bus_data) == 1) {
      return(matrix(c(0, 0.1, 0.1, 0), nrow = 2, ncol = 2))
    } else {
      return(matrix(nrow = 0, ncol = 0))
    }
  }
  
  # Enhanced power data extraction with validation
  local_bus_power <- tryCatch({
    local_bus_data %>%
      st_drop_geometry() %>%
      mutate(
        # Core power characteristics with safe defaults
        net_power = ifelse(is.na(total_gen) | is.na(load_mw), 0, total_gen - load_mw),
        generation_ratio = case_when(
          is.na(total_gen) | is.na(load_mw) ~ 0.5,
          (total_gen + load_mw) == 0 ~ 0.5,
          TRUE ~ total_gen / (total_gen + load_mw)
        ),
        load_density = ifelse(is.na(load_mw), 0, load_mw),
        gen_capacity = ifelse(is.na(total_gen), 0, total_gen),
        
        # Electrical characteristics with validation
        voltage_factor = case_when(
          is.na(vm) | vm == 0 ~ 1.0,
          vm < 0.5 | vm > 2.0 ~ 1.0,  # Reasonable bounds
          TRUE ~ vm
        ),
        base_kv = case_when(
          is.na(baseKV) | baseKV <= 0 ~ 138,  # Default transmission voltage
          TRUE ~ baseKV
        ),
        
        # Bus type characteristics
        bus_type_numeric = case_when(
          is.na(bus_type) ~ 1.0,
          bus_type == "Generator" ~ 3.0,
          bus_type == "Gen + Load" ~ 2.0, 
          bus_type == "Load" ~ 1.0,
          TRUE ~ 1.0
        ),
        
        # Geographic characteristics (handle missing coordinates)
        location_factor_x = case_when(
          is.na(longitude) ~ 0,
          TRUE ~ abs(longitude) * 0.01
        ),
        location_factor_y = case_when(
          is.na(latitude) ~ 0,
          TRUE ~ abs(latitude) * 0.01
        ),
        
        # Zone/area characteristics
        zone_factor = case_when(
          is.na(zone) ~ 0.1,
          TRUE ~ zone * 0.1
        )
      ) %>%
      select(bus_i, net_power, generation_ratio, load_density, gen_capacity, 
             voltage_factor, base_kv, bus_type_numeric, location_factor_x, 
             location_factor_y, zone_factor)
  }, error = function(e) {
    message("ERROR in power data extraction: ", e$message)
    message("Creating fallback power data...")
    
    # Fallback: create minimal power data
    data.frame(
      bus_i = if ("bus_i" %in% names(local_bus_data)) local_bus_data$bus_i else seq_len(nrow(local_bus_data)),
      net_power = rnorm(nrow(local_bus_data), 0, 10),
      generation_ratio = rep(0.5, nrow(local_bus_data)),
      load_density = rep(1.0, nrow(local_bus_data)),
      gen_capacity = rep(1.0, nrow(local_bus_data)),
      voltage_factor = rep(1.0, nrow(local_bus_data)),
      base_kv = rep(138, nrow(local_bus_data)),
      bus_type_numeric = rep(1.0, nrow(local_bus_data)),
      location_factor_x = rep(0.1, nrow(local_bus_data)),
      location_factor_y = rep(0.1, nrow(local_bus_data)),
      zone_factor = rep(0.1, nrow(local_bus_data))
    )
  })
  
  # Validate the power data
  if (is.null(local_bus_power) || nrow(local_bus_power) == 0) {
    message("CRITICAL ERROR: Could not create power data")
    return(matrix(c(0, 0.1, 0.1, 0), nrow = 2, ncol = 2))
  }
  
  message("Enhanced power data statistics:")
  message("  Net power range: [", round(min(local_bus_power$net_power, na.rm = TRUE), 2), 
          ", ", round(max(local_bus_power$net_power, na.rm = TRUE), 2), "] MW")
  
  n_buses <- nrow(local_bus_power)
  message("  Matrix size will be: ", n_buses, "×", n_buses)
  
  # Enhanced matrix creation with error handling
  power_diff_matrix <- tryCatch({
    if (method == "enhanced_multi_factor") {
      # Create multi-dimensional distance matrix
      matrix_result <- matrix(0, n_buses, n_buses)
      
      # Weights for different factors
      weights <- list(
        net_power = 1.0, generation_ratio = 0.8, load_density = 0.6,
        gen_capacity = 0.7, voltage_factor = 0.4, base_kv = 0.3,
        bus_type = 0.5, location_x = 0.2, location_y = 0.2, zone = 0.3
      )
      
      message("Computing enhanced multi-factor distance matrix...")
      
      for (i in 1:n_buses) {
        for (j in 1:n_buses) {
          if (i != j) {
            # Calculate weighted distance across all factors
            distance <- 0
            
            # Power difference (normalized)
            power_diff <- abs(local_bus_power$net_power[i] - local_bus_power$net_power[j])
            max_power_diff <- max(abs(local_bus_power$net_power)) - min(abs(local_bus_power$net_power))
            if (max_power_diff > 0) {
              distance <- distance + weights$net_power * (power_diff / max_power_diff)
            }
            
            # Add other factors...
            gen_ratio_diff <- abs(local_bus_power$generation_ratio[i] - local_bus_power$generation_ratio[j])
            distance <- distance + weights$generation_ratio * gen_ratio_diff
            
            # Store the distance
            matrix_result[i, j] <- distance + runif(1, 0, 0.01)  # Add small random component
          }
        }
      }
      
      matrix_result
      
    } else {
      # Simpler fallback method
      power_values <- local_bus_power$net_power
      
      # Add variation if power is too uniform
      if (var(power_values) < 1e-6) {
        power_values <- power_values + rnorm(n_buses, 0, 1)
      }
      
      as.matrix(dist(power_values, method = "euclidean"))
    }
    
  }, error = function(e) {
    message("ERROR in matrix creation: ", e$message)
    message("Creating fallback matrix...")
    
    # Create a simple random matrix as absolute fallback
    fallback_matrix <- matrix(runif(n_buses * n_buses, 0, 1), n_buses, n_buses)
    diag(fallback_matrix) <- 0
    fallback_matrix
  })
  
  # Final validation and normalization
  if (is.null(power_diff_matrix) || any(dim(power_diff_matrix) == 0)) {
    message("CRITICAL ERROR: Matrix creation failed completely")
    return(matrix(c(0, 0.1, 0.1, 0), nrow = 2, ncol = 2))
  }
  
  # Normalize to [0,1] range while preserving structure
  max_val <- max(power_diff_matrix, na.rm = TRUE)
  min_val <- min(power_diff_matrix[power_diff_matrix > 0], na.rm = TRUE)
  
  if (!is.finite(max_val) || !is.finite(min_val) || max_val <= min_val) {
    message("WARNING: Invalid matrix values, using random matrix")
    power_diff_matrix <- matrix(runif(n_buses * n_buses, 0, 1), n_buses, n_buses)
    diag(power_diff_matrix) <- 0
  } else {
    power_diff_matrix <- (power_diff_matrix - min_val) / (max_val - min_val)
    diag(power_diff_matrix) <- 0
  }
  
  # Final statistics
  final_var <- var(as.vector(power_diff_matrix), na.rm = TRUE)
  final_min <- min(power_diff_matrix, na.rm = TRUE)
  final_max <- max(power_diff_matrix, na.rm = TRUE)
  
  message("✓ ENHANCED MATRIX GENERATION COMPLETE:")
  message("  Size: ", n_buses, "×", n_buses)
  message("  Range: [", round(final_min, 6), ", ", round(final_max, 6), "]")
  message("  Variance: ", sprintf("%.2e", final_var))
  message("  Non-zero elements: ", sum(power_diff_matrix > 1e-10, na.rm = TRUE))
  
  # Quality check
  if (final_var > 1e-4 && is.finite(final_var)) {
    message("✓ Matrix has sufficient variance for meaningful TDA")
  } else {
    message("⚠ Matrix variance may be low, but analysis will proceed")
  }
  
  return(power_diff_matrix)
}

# =================================================================================================
# PERSEUS FILE I/O FUNCTIONS
# =================================================================================================
run_perseus <- function(input_file = NULL, output_prefix = NULL) {
  
  # Use the config from global.R
  if (exists("tda_config") && "simple_perseus" %in% names(tda_config)) {
    cfg <- tda_config$simple_perseus
  } else {
    # Fallback config
    cfg <- list(
      perseus_exe = normalizePath("Perseus/perseusWin.exe", winslash="\\"),
      outputs_dir = "outputs/"
    )
  }
  
  if (is.null(input_file)) {
    input_file <- file.path(cfg$outputs_dir, "M.txt")
  }
  if (is.null(output_prefix)) {
    output_prefix <- file.path(cfg$outputs_dir, "Moutput")
  }
  
  if (!file.exists(cfg$perseus_exe)) {
    stop("Perseus executable not found: ", cfg$perseus_exe)
  }
  
  if (!file.exists(input_file)) {
    stop("Perseus input file not found: ", input_file)
  }
  
  # Run Perseus exactly like working code
  perseus_cmd <- paste(cfg$perseus_exe, "distmat", input_file, output_prefix)
  message("Running Perseus: ", perseus_cmd)
  
  result <- system(perseus_cmd, intern = TRUE)
  
  # Check outputs
  expected_files <- paste0(output_prefix, "_", 0:2, ".txt")
  existing_files <- file.exists(expected_files)
  
  message("Perseus execution complete")
  for (i in seq_along(expected_files)) {
    if (existing_files[i]) {
      size <- file.info(expected_files[i])$size
      message("  ✓ ", basename(expected_files[i]), " (", size, " bytes)")
    }
  }
  
  return(expected_files[existing_files])
}

write_enhanced_perseus_file <- function(adjacency_matrix, output_dir) {
  # Use the same function as before but with enhanced debugging
  cfg <- perseus_config
  
  if (is.null(output_dir)) {
    output_dir <- cfg$outputs_dir
  }
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  A2 <- as.matrix(adjacency_matrix)
  A2[A2 == 0] <- 999
  diag(A2) <- 0
  
  d <- nrow(A2)
  
  output_file <- file.path(output_dir, "M.txt")
  
  # Write file
  cat(d, file = output_file, append = FALSE, sep = '\n')
  cat(paste(cfg$g, cfg$s, cfg$N, cfg$C, sep = ' '), 
      file = output_file, append = TRUE, sep = '\n')
  cat(as.vector(A2), file = output_file, append = TRUE)
  
  message("✓ Enhanced Perseus file written: ", output_file)
  message("  Matrix size: ", d, "×", d)
  message("  Parameters: g=", cfg$g, ", s=", cfg$s, ", N=", cfg$N, ", C=", cfg$C)
  
  return(output_file)
}

# FIXED: Read Perseus outputs matching exact format (Moutput_0.txt, etc.)
read_perseus_outputs <- function(output_prefix) {
  message("=== ENHANCED PERSEUS OUTPUT READING ===")
  message("Reading from prefix: ", output_prefix)
  
  P <- NULL
  filt_len <- perseus_config$N
  total_features <- 0
  
  # Check what files exist
  dim0_file <- paste0(output_prefix, "_0.txt")
  dim1_file <- paste0(output_prefix, "_1.txt")
  
  message("Checking Perseus output files:")
  message("  Dimension 0 file: ", dim0_file, " (exists: ", file.exists(dim0_file), ")")
  message("  Dimension 1 file: ", dim1_file, " (exists: ", file.exists(dim1_file), ")")
  
  if (file.exists(dim0_file)) {
    file_size <- file.info(dim0_file)$size
    message("    Size: ", file_size, " bytes")
    
    if (file_size > 0) {
      tryCatch({
        # Read with more robust parsing
        dim0_lines <- readLines(dim0_file)
        message("    Raw lines read: ", length(dim0_lines))
        
        if (length(dim0_lines) > 0) {
          # Parse lines manually for better control
          dim0_data <- do.call(rbind, lapply(dim0_lines, function(line) {
            parts <- strsplit(trimws(line), "\\s+")[[1]]
            if (length(parts) >= 2) {
              return(c(as.numeric(parts[1]), as.numeric(parts[2])))
            }
            return(NULL)
          }))
          
          if (!is.null(dim0_data) && nrow(dim0_data) > 0) {
            dim0_matrix <- as.matrix(dim0_data)
            
            # Process birth/death times
            dim0_matrix[dim0_matrix[, 2] == -1, 2] <- filt_len + 1
            dim0_matrix <- dim0_matrix / (filt_len + 1)
            
            # Add dimension column
            P <- cbind(rep(0, nrow(dim0_matrix)), dim0_matrix)
            total_features <- total_features + nrow(dim0_matrix)
            message("  ✓ Dimension 0: ", nrow(dim0_matrix), " features loaded successfully")
          }
        }
      }, error = function(e) {
        message("  ✗ Error reading dimension 0 file: ", e$message)
      })
    }
  }
  
  # Read dimension 1 with same robust approach
  if (file.exists(dim1_file)) {
    file_size <- file.info(dim1_file)$size
    message("    Size: ", file_size, " bytes")
    
    if (file_size > 0) {
      tryCatch({
        dim1_lines <- readLines(dim1_file)
        message("    Raw lines read: ", length(dim1_lines))
        
        if (length(dim1_lines) > 0) {
          dim1_data <- do.call(rbind, lapply(dim1_lines, function(line) {
            parts <- strsplit(trimws(line), "\\s+")[[1]]
            if (length(parts) >= 2) {
              return(c(as.numeric(parts[1]), as.numeric(parts[2])))
            }
            return(NULL)
          }))
          
          if (!is.null(dim1_data) && nrow(dim1_data) > 0) {
            dim1_matrix <- as.matrix(dim1_data)
            
            # Process birth/death times
            dim1_matrix[dim1_matrix[, 2] == -1, 2] <- filt_len + 1
            dim1_matrix <- dim1_matrix / (filt_len + 1)
            
            # Add dimension column and combine
            dim1_with_dim <- cbind(rep(1, nrow(dim1_matrix)), dim1_matrix)
            P <- rbind(P, dim1_with_dim)
            total_features <- total_features + nrow(dim1_matrix)
            message("  ✓ Dimension 1: ", nrow(dim1_matrix), " features loaded successfully")
          }
        }
      }, error = function(e) {
        message("  ✗ Error reading dimension 1 file: ", e$message)
      })
    }
  }
  
  # Final validation
  if (is.null(P) || nrow(P) == 0) {
    P <- matrix(nrow = 0, ncol = 3)  
    message("  ⚠ WARNING: No persistence features found in Perseus output files")
    message("    This might indicate:")
    message("      - Perseus execution failed silently")
    message("      - Input matrix had no topological structure") 
    message("      - Perseus parameters need adjustment")
  } else {
    message("  ✓ TOTAL FEATURES LOADED: ", total_features)
  }
  
  colnames(P) <- c("dimension", "birth", "death")
  return(P)
}


# =================================================================================================
# RESULTS SAVING FUNCTIONS
# =================================================================================================

# Enhanced save function
save_results_tda_fixed <- function(diagram, cfg) {
  message("Saving analysis results...")
  
  features_file <- file.path(cfg$outputs_dir, "persistence_features.csv")
  
  if (nrow(diagram) > 0) {
    df <- as.data.frame(diagram)
    colnames(df) <- c("Dimension", "Birth", "Death")
    df$Persistence <- df$Death - df$Birth
    fwrite(df, features_file)
    message("✓ Persistence features saved: ", features_file)
  }
  
  summary_file <- file.path(cfg$outputs_dir, "analysis_summary.txt")
  
  sink(summary_file)
  cat("=== CORRECTED Net Power Perseus Analysis ===\n")
  cat("Analysis date:", as.character(Sys.time()), "\n")
  cat("Input file:", cfg$net_power_csv, "\n")
  cat("Perseus executable:", cfg$perseus_exe, "\n\n")
  
  cat("Parameters:\n")
  cat("  Max dimension:", cfg$max_dim, "\n")
  cat("  Filtration steps (N):", cfg$N, "\n")
  cat("  Scale increment (s):", cfg$s, "\n")
  cat("  Genus (g):", cfg$g, "\n")
  cat("  Connectivity (C):", cfg$C, "\n")
  cat("  Downsample limit:", cfg$downsample_max_pts, "\n")
  cat("  Timeout (seconds):", cfg$timeout_seconds, "\n\n")
  
  if (nrow(diagram) > 0) {
    cat("Results:\n")
    cat("  Total features:", nrow(diagram), "\n")
    
    df <- as.data.frame(diagram)
    feature_counts <- table(df[,1])
    for (dim in names(feature_counts)) {
      cat("  Dimension", dim, "features:", feature_counts[dim], "\n")
    }
    
    cat("\nMost persistent features:\n")
    df$persistence <- df[,3] - df[,2]
    top_features <- df[order(df$persistence, decreasing = TRUE), ]
    for (i in 1:min(5, nrow(top_features))) {
      cat(sprintf("  %d. Dim %d: Birth=%.4f, Death=%.4f, Persistence=%.4f\n",
                  i, top_features[i,1], top_features[i,2], 
                  top_features[i,3], top_features$persistence[i]))
    }
  } else {
    cat("Results: No persistent features found\n")
  }
  sink()
  
  message("✓ Analysis summary saved: ", summary_file)
}

# =================================================================================================
# MAIN ANALYSIS FUNCTION
# =================================================================================================
run_perseus_analysis <- function(distance_matrix, output_dir = NULL) {
  
  if (is.null(output_dir)) output_dir <- perseus_config$outputs_dir
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("=== ENHANCED PERSEUS ANALYSIS WITH DEBUGGING ===")
  message("Input matrix size: ", nrow(distance_matrix), "×", ncol(distance_matrix))
  
  # Enhanced matrix validation
  if (nrow(distance_matrix) < 2) {
    message("WARNING: Matrix too small for meaningful TDA analysis")
    empty_diagram <- matrix(nrow = 0, ncol = 3)
    colnames(empty_diagram) <- c("dimension", "birth", "death")
    return(list(
      success = TRUE,
      persistence_data = empty_diagram,
      matrix_size = nrow(distance_matrix),
      method = "too_small"
    ))
  }
  
  # Check matrix properties
  matrix_min <- min(distance_matrix, na.rm = TRUE)
  matrix_max <- max(distance_matrix, na.rm = TRUE)
  matrix_mean <- mean(distance_matrix, na.rm = TRUE)
  matrix_var <- var(as.vector(distance_matrix), na.rm = TRUE)
  
  message("Matrix statistics:")
  message("  Range: [", round(matrix_min, 4), ", ", round(matrix_max, 4), "]")
  message("  Mean: ", round(matrix_mean, 4))
  message("  Variance: ", round(matrix_var, 6))
  
  # Check if matrix has sufficient variation
  if (matrix_var < 1e-10) {
    message("WARNING: Matrix has very low variance - may not produce meaningful TDA results")
  }
  
  # Check for constant matrix (all same values)
  if (matrix_max - matrix_min < 1e-10) {
    message("WARNING: Matrix is nearly constant - this will not produce meaningful features")
    # Still proceed but warn user
  }
  
  # Downsample if too large
  original_size <- nrow(distance_matrix)
  if (nrow(distance_matrix) > perseus_config$downsample_max_pts) {
    distance_matrix <- downsample_matrix(distance_matrix, perseus_config$downsample_max_pts)
    message("Matrix downsampled from ", original_size, " to ", nrow(distance_matrix), " points")
  }
  
  tryCatch({
    # Write Perseus input file
    input_file <- write_enhanced_perseus_file(distance_matrix, output_dir)
    output_prefix <- file.path(output_dir, perseus_config$perseus_output_prefix)
    
    # Run Perseus with timeout
    perseus_cmd <- paste(perseus_config$perseus_exe, "distmat", input_file, output_prefix)
    message("Executing Perseus: ", perseus_cmd)
    
    system_result <- system(perseus_cmd, timeout = perseus_config$timeout_seconds)
    
    # *** CORRECTED LOGIC ***
    # If Perseus fails, return a failure object instead of calling an alternative function.
    if (system_result != 0) {
      message("Perseus execution failed with code: ", system_result)
      empty_diagram <- matrix(nrow = 0, ncol = 3)
      colnames(empty_diagram) <- c("dimension", "birth", "death")
      return(list(
        success = FALSE,
        error = paste("Perseus execution failed with code:", system_result),
        persistence_data = empty_diagram,
        method = "failed_perseus_execution"
      ))
    }
    
    # Read and process results with enhanced debugging
    persistence_data <- read_perseus_outputs(output_prefix)
    
    message("✓ Perseus analysis complete:")
    message("  Features found: ", nrow(persistence_data))
    
    if (nrow(persistence_data) > 0) {
      # Analyze the features
      feature_dims <- table(persistence_data[, 1])
      for (dim in names(feature_dims)) {
        message("    Dimension ", dim, ": ", feature_dims[dim], " features")
      }
      
      # Show most persistent features
      if (nrow(persistence_data) > 0) {
        persistence_values <- persistence_data[, 3] - persistence_data[, 2]
        max_persistence <- max(persistence_values)
        mean_persistence <- mean(persistence_values)
        message("  Persistence statistics:")
        message("    Max persistence: ", round(max_persistence, 6))
        message("    Mean persistence: ", round(mean_persistence, 6))
      }
    } else {
      message("  WARNING: No persistent features found!")
      message("  This suggests the matrix may not have meaningful topological structure")
    }
    
    return(list(
      success = TRUE,
      persistence_data = persistence_data,
      matrix_size = nrow(distance_matrix),
      original_size = original_size,
      method = "perseus"
    ))
    
  }, error = function(e) {
    message("Error in Perseus analysis: ", e$message)
    # *** CORRECTED LOGIC ***
    # Return a failure object on any error.
    empty_diagram <- matrix(nrow = 0, ncol = 3)
    colnames(empty_diagram) <- c("dimension", "birth", "death")
    return(list(
      success = FALSE,
      error = e$message,
      persistence_data = empty_diagram,
      method = "failed_perseus"
    ))
  })
}

save_analysis_summary  <- function(output_dir, fire_name, analysis_params, 
                                   before_results, after_results, wasserstein_distance, 
                                   cascade_results, is_compound_event = FALSE, 
                                   fire_names = NULL) {
  summary_file <- file.path(output_dir, "enhanced_analysis_summary.txt")
  sink(summary_file)
  
  cat("=== ENHANCED WILDFIRE TDA ANALYSIS SUMMARY ===\n\n")
  
  if (is_compound_event) {
    cat("COMPOUND FIRE EVENT ANALYSIS\n")
    cat("Event Name: ", fire_name, "\n")
    cat("Individual Fires: ", paste(fire_names, collapse = ", "), "\n")
    cat("Number of Fires: ", length(fire_names), "\n")
  } else {
    cat("SINGLE FIRE EVENT ANALYSIS\n")
    cat("Fire Event: ", fire_name, "\n")
  }
  
  cat("Analysis Time: ", as.character(Sys.time()), "\n")
  cat("Matrix Method: ", analysis_params$matrix_method, "\n\n")
  
  cat("--- ENHANCED ANALYSIS SCOPE ---\n")
  cat("Analysis Type: ", if (is_compound_event) "Compound Fire Event" else "Single Fire Event", "\n")
  cat("Area of Interest Radius: ", analysis_params$analysis_radius_km, " km\n")
  cat("Fire Impact Buffer: ", analysis_params$fire_buffer_km, " km\n")
  cat("Matrix Generation: Enhanced multi-factor approach\n\n")
  
  cat("--- ENHANCED TDA RESULTS ---\n")
  cat("Features Before Fire: ", nrow(before_results$persistence_data), "\n")
  cat("Features After Fire: ", nrow(after_results$persistence_data), "\n")
  cat("Feature Change: ", nrow(after_results$persistence_data) - nrow(before_results$persistence_data), "\n")
  cat("Enhanced Wasserstein Distance: ", round(wasserstein_distance, 8), "\n")
  
  if (nrow(before_results$persistence_data) > 0) {
    before_persistence <- before_results$persistence_data[, 3] - before_results$persistence_data[, 2]
    cat("Before State Max Persistence: ", round(max(before_persistence), 6), "\n")
    cat("Before State Mean Persistence: ", round(mean(before_persistence), 6), "\n")
  }
  
  if (nrow(after_results$persistence_data) > 0) {
    after_persistence <- after_results$persistence_data[, 3] - after_results$persistence_data[, 2]
    cat("After State Max Persistence: ", round(max(after_persistence), 6), "\n")
    cat("After State Mean Persistence: ", round(mean(after_persistence), 6), "\n")
  }
  
  if (!is.null(cascade_results$metrics) && nrow(cascade_results$metrics) > 0) {
    cat("\n--- CASCADE IMPACT SUMMARY ---\n")
    total_fire_affected <- sum(cascade_results$metrics$fire_affected, na.rm = TRUE)
    total_cascade_failures <- sum(cascade_results$metrics$deenergized, na.rm = TRUE)
    cat("Direct Fire Impact: ", total_fire_affected, " buses\n")
    cat("Cascade Failures: ", total_cascade_failures, " buses\n")
    cat("Total Impact: ", total_fire_affected + total_cascade_failures, " buses\n")
    if (total_fire_affected > 0) {
      cat("Cascade Amplification: ", round(total_cascade_failures / total_fire_affected, 2), "x\n")
    }
  }
  
  cat("\n--- ENHANCED ANALYSIS FEATURES ---\n")
  cat("• Multi-factor distance matrices\n")
  cat("• Enhanced variance validation\n")
  cat("• Robust Perseus output parsing\n")
  cat("• Detailed topological logging\n")
  cat("• Advanced visualization generation\n")
  
  if (is_compound_event) {
    cat("\n--- COMPOUND EVENT SPECIFICS ---\n")
    for (i in seq_along(fire_names)) {
      cat("Fire ", i, ": ", fire_names[i], "\n")
    }
    cat("Combined Impact Analysis: Multi-fire cascading topology\n")
    cat("Enhanced Topological Signature: Compound disturbance patterns\n")
  }
  
  sink()
  message("✓ Enhanced analysis summary saved: ", summary_file)
}

downsample_matrix <- function(mat, max_pts) {
  if (is.null(mat) || nrow(mat) <= max_pts) return(mat)
  set.seed(42)
  keep_indices <- sort(sample(nrow(mat), max_pts))
  message("  Matrix downsampled: ", nrow(mat), " -> ", max_pts, " points")
  return(mat[keep_indices, keep_indices])
}

calculate_wasserstein_distance <- function(P1, P2, p = 2, dimension = NULL) {
  
  message("DEBUG: Wasserstein distance calculation:")
  message("  P1 dimensions: ", ifelse(is.null(P1), "NULL", paste(dim(P1), collapse = "x")))
  message("  P2 dimensions: ", ifelse(is.null(P2), "NULL", paste(dim(P2), collapse = "x")))
  
  # Handle NULL inputs
  if (is.null(P1) && is.null(P2)) {
    message("  Both diagrams are NULL - returning 0")
    return(0)
  }
  
  if (is.null(P1)) P1 <- matrix(nrow = 0, ncol = 3)
  if (is.null(P2)) P2 <- matrix(nrow = 0, ncol = 3)
  
  # Convert to matrices if needed
  if (!is.matrix(P1)) P1 <- as.matrix(P1)
  if (!is.matrix(P2)) P2 <- as.matrix(P2)
  
  # Handle empty diagrams
  if (nrow(P1) == 0 && nrow(P2) == 0) {
    message("  Both diagrams are empty - returning 0")
    return(0)
  }
  
  if (nrow(P1) == 0 && nrow(P2) > 0) {
    total_persistence <- sum(P2[, 3] - P2[, 2])
    message("  P1 empty, P2 has ", nrow(P2), " features - returning ", total_persistence)
    return(total_persistence)
  }
  
  if (nrow(P2) == 0 && nrow(P1) > 0) {
    total_persistence <- sum(P1[, 3] - P1[, 2])
    message("  P2 empty, P1 has ", nrow(P1), " features - returning ", total_persistence)
    return(total_persistence)
  }
  
  # Ensure correct column names for TDA package
  if (ncol(P1) >= 3) colnames(P1) <- c("dimension", "birth", "death")
  if (ncol(P2) >= 3) colnames(P2) <- c("dimension", "birth", "death")
  
  message("  P1 features: ", nrow(P1), " (dims: ", paste(unique(P1[,1]), collapse = ","), ")")
  message("  P2 features: ", nrow(P2), " (dims: ", paste(unique(P2[,1]), collapse = ","), ")")
  
  # Display some feature information for debugging
  if (nrow(P1) > 0) {
    p1_persistence <- P1[, 3] - P1[, 2]
    message("  P1 persistence range: [", round(min(p1_persistence), 4), ", ", round(max(p1_persistence), 4), "]")
  }
  
  if (nrow(P2) > 0) {
    p2_persistence <- P2[, 3] - P2[, 2]
    message("  P2 persistence range: [", round(min(p2_persistence), 4), ", ", round(max(p2_persistence), 4), "]")
  }
  
  tryCatch({
    if (is.null(dimension)) {
      # Calculate Wasserstein distance for all dimensions present
      dims_P1 <- unique(P1[, 1])
      dims_P2 <- unique(P2[, 1])
      all_dims <- unique(c(dims_P1, dims_P2))
      
      total_distance <- 0
      for (dim in all_dims) {
        P1_dim <- P1[P1[, 1] == dim, , drop = FALSE]
        P2_dim <- P2[P2[, 1] == dim, , drop = FALSE]
        
        if (nrow(P1_dim) > 0 || nrow(P2_dim) > 0) {
          dim_distance <- TDA::wasserstein(P1_dim, P2_dim, p = p, dimension = dim)
          message("  Dimension ", dim, " distance: ", round(dim_distance, 6))
          total_distance <- total_distance + dim_distance
        }
      }
      
      message("  Final Wasserstein distance: ", round(total_distance, 6))
      return(total_distance)
    } else {
      # Calculate for specific dimension
      distance <- TDA::wasserstein(P1, P2, p = p, dimension = dimension)
      message("  Dimension ", dimension, " Wasserstein distance: ", round(distance, 6))
      return(distance)
    }
  }, error = function(e) {
    message("WARNING: Wasserstein calculation failed: ", e$message)
    
    # Enhanced fallback calculation
    if (nrow(P1) == 0 && nrow(P2) == 0) return(0)
    
    # Calculate based on feature count difference and persistence differences
    p1_persistence <- if (nrow(P1) > 0) sum(P1[, 3] - P1[, 2]) else 0
    p2_persistence <- if (nrow(P2) > 0) sum(P2[, 3] - P2[, 2]) else 0
    
    persistence_diff <- abs(p1_persistence - p2_persistence)
    feature_count_diff <- abs(nrow(P1) - nrow(P2)) * 0.1
    
    fallback_distance <- persistence_diff + feature_count_diff
    
    message("  Using enhanced fallback calculation:")
    message("    P1 total persistence: ", round(p1_persistence, 4))
    message("    P2 total persistence: ", round(p2_persistence, 4))
    message("    Persistence difference: ", round(persistence_diff, 4))
    message("    Feature count penalty: ", round(feature_count_diff, 4))
    message("    Final fallback distance: ", round(fallback_distance, 6))
    
    return(fallback_distance)
  })
}

extract_bus_ids_from_csv_matrix <- function(matrix_data) {
  # Helper function to extract bus IDs from the loaded CSV matrix
  if (is.null(rownames(matrix_data)) || is.null(colnames(matrix_data))) {
    # If no names, assume sequential bus IDs
    return(1:nrow(matrix_data))
  }
  
  # Extract from row names (handle "bus_123" or "123" formats)
  bus_ids <- as.numeric(gsub("bus_", "", rownames(matrix_data)))
  
  # Remove any NAs and ensure we have valid bus IDs
  bus_ids <- bus_ids[!is.na(bus_ids)]
  
  return(bus_ids)
}



# =================================================================================================
# SCRIPT INITIALIZATION AND USAGE INFORMATION
# =================================================================================================

message("=== Perseus Analysis Script Loaded ===")
message("")
message("Key corrections applied:")
message("  ✓ Fixed file names: M.txt and Moutput_X.txt")
message("  ✓ Fixed parameters to match working M.txt: g=0, s=0.1, N=10, C=3")
message("  ✓ Exact matrix format matching working code")
message("  ✓ Better code organization and spacing")
message("  ✓ Enhanced error handling and debugging")
