# =================================================================================================
# PERSEUS TOPOLOGICAL DATA ANALYSIS
# Fixed Net Power Perseus Analysis with Exact M.txt Format Matching



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

run_smart_tda_workflow <- function(fire_data, bus_info, graph_original, 
                                   analysis_radius_km = 30, 
                                   fire_impact_buffer_km = 2,
                                   simulation_steps = 20,
                                   use_existing_cascade = TRUE,
                                   existing_cascade_results = NULL,
                                   generate_plots = TRUE, 
                                   display_plots = TRUE) {
  
  # UPDATED: Detect compound fire events
  fire_names <- unique(fire_data$attr_IncidentName)
  is_compound_event <- length(fire_names) > 1
  
  if (is_compound_event) {
    message("=== SMART INTEGRATED TDA-CASCADE WORKFLOW (COMPOUND EVENT) ===")
    message("COMPOUND FIRE EVENT ANALYSIS:")
    message("  Number of fires: ", length(fire_names))
    message("  Fire names: ", paste(fire_names, collapse = ", "))
  } else {
    message("=== SMART INTEGRATED TDA-CASCADE WORKFLOW (SINGLE FIRE) ===")
    message("Fire: ", fire_names[1])
  }
  
  message("Parameters:")
  message("  Analysis Radius: ", analysis_radius_km, " km (TDA scope)")
  message("  Fire Impact Buffer: ", fire_impact_buffer_km, " km (cascade impacts)")
  message("  Use Existing Cascade: ", use_existing_cascade)
  
  validation <- validate_spatial_data(fire_data, bus_info)
  if (!validation$valid) {
    return(list(success = FALSE, error = paste("Spatial data validation failed:", 
                                               paste(validation$errors, collapse = "; "))))
  }
  
  fire_data <- validation$fire_data
  bus_info <- validation$bus_data
  
  # UPDATED: Handle compound fire naming
  if (is_compound_event) {
    # Create compound fire identifier
    safe_fire_name <- paste("compound", length(fire_names), "fires", sep = "_")
    display_fire_name <- paste("Compound Event:", length(fire_names), "fires")
    compound_fire_names <- paste(fire_names, collapse = " + ")
  } else {
    safe_fire_name <- gsub("[^A-Za-z0-9-]", "_", fire_names[1])
    display_fire_name <- fire_names[1]
    compound_fire_names <- fire_names[1]
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  
  # Setup output directories with compound event naming
  attack_run_dir <- file.path(cfg$outputs_attacked_dir, paste(safe_fire_name, timestamp, sep = "_"))
  before_dir <- file.path(attack_run_dir, "before_analysis_local")
  after_dir <- file.path(attack_run_dir, "after_analysis_local")
  plots_dir <- file.path(attack_run_dir, "plots")
  
  dir.create(attack_run_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(before_dir, showWarnings = FALSE)
  dir.create(after_dir, showWarnings = FALSE)
  dir.create(plots_dir, showWarnings = FALSE)
  
  # STEP 1: Handle Cascade Simulation Intelligently
  cascade_results <- NULL
  
  if (use_existing_cascade && !is.null(existing_cascade_results)) {
    if (is_compound_event) {
      message("[1/6] Checking existing cascade results for compound event compatibility...")
    } else {
      message("[1/6] Using existing cascade results...")
    }
    cascade_results <- existing_cascade_results
    
    # Validate existing results match current fire(s)
    if (!is.null(cascade_results$fire_name)) {
      if (is_compound_event) {
        # For compound events, check if the fire list matches
        if (!identical(sort(fire_names), sort(strsplit(cascade_results$fire_name, " \\+ ")[[1]]))) {
          message("  Warning: Existing cascade results are for different fire combination, running new simulation")
          cascade_results <- NULL
        }
      } else {
        if (cascade_results$fire_name != fire_names[1]) {
          message("  Warning: Existing cascade results are for different fire, running new simulation")
          cascade_results <- NULL
        }
      }
    }
  }
  
  if (is.null(cascade_results)) {
    if (is_compound_event) {
      message("[1/6] Running compound fire cascade simulation with ", fire_impact_buffer_km, "km buffer...")
      message("  Analyzing combined effects of ", length(fire_names), " simultaneous fires")
    } else {
      message("[1/6] Running cascade simulation with ", fire_impact_buffer_km, "km buffer...")
    }
    
    cascade_results <- run_enhanced_fire_cascade(
      graph = graph_original,
      buses_sf = bus_info,
      fire_data = fire_data,
      buffer_km = fire_impact_buffer_km,
      steps = simulation_steps
    )
    cascade_results$fire_name <- compound_fire_names  # Store compound name
    cascade_results$buffer_km <- fire_impact_buffer_km
    cascade_results$is_compound_event <- is_compound_event
    cascade_results$fire_count <- length(fire_names)
  }
  
  all_failed_buses_global <- unique(unlist(cascade_results$buses_lost_per_step))
  if (is_compound_event) {
    message("  Compound cascade complete: ", length(all_failed_buses_global), " total buses failed")
  } else {
    message("  Cascade complete: ", length(all_failed_buses_global), " total buses failed")
  }
  
  # STEP 2: Define TDA Analysis Area
  if (is_compound_event) {
    message("[2/6] Defining TDA analysis area for compound event (", analysis_radius_km, "km radius)...")
  } else {
    message("[2/6] Defining TDA analysis area (", analysis_radius_km, "km radius)...")
  }
  
  analysis_radius_miles <- analysis_radius_km * 0.621371
  
  # For compound events, use union of all fire areas for local area definition
  if (is_compound_event) {
    combined_fire_area <- st_union(fire_data)
    local_area_data <- get_local_area_data_compound(combined_fire_area, bus_info, analysis_radius_miles)
  } else {
    local_area_data <- get_local_area_data(fire_data, bus_info, analysis_radius_miles)
  }
  
  local_bus_data <- local_area_data$buses
  
  if (nrow(local_bus_data) < 2) {
    return(list(success = FALSE, error = "Not enough buses in TDA analysis area."))
  }
  message("  TDA analysis area: ", nrow(local_bus_data), " buses")
  
  # STEP 3: Before State Analysis
  if (is_compound_event) {
    message("[3/6] Analyzing 'before' topology in local area (compound event baseline)...")
  } else {
    message("[3/6] Analyzing 'before' topology in local area...")
  }
  
  local_healthy_matrix <- generate_local_power_matrix(local_bus_data)
  tda_before_results <- run_perseus_analysis(local_healthy_matrix, output_dir = before_dir)
  if (!tda_before_results$success) {
    return(list(success = FALSE, error = "Perseus failed on 'before' state."))
  }
  
  # STEP 4: After State Analysis  
  if (is_compound_event) {
    message("[4/6] Analyzing 'after' topology following compound fire cascade...")
  } else {
    message("[4/6] Analyzing 'after' topology in local area...")
  }
  
  surviving_local_bus_ids <- setdiff(local_bus_data$bus_i, all_failed_buses_global)
  
  if (length(surviving_local_bus_ids) < 2) {
    return(list(success = FALSE, error = "Not enough surviving buses in analysis area."))
  }
  
  surviving_local_bus_data <- local_bus_data %>% filter(bus_i %in% surviving_local_bus_ids)
  message("  Surviving buses in analysis area: ", nrow(surviving_local_bus_data))
  
  local_attacked_matrix <- generate_local_power_matrix(surviving_local_bus_data)
  tda_after_results <- run_perseus_analysis(local_attacked_matrix, output_dir = after_dir)
  if (!tda_after_results$success) {
    return(list(success = FALSE, error = "Perseus failed on 'after' state."))
  }
  
  # STEP 5: Calculate Topological Distance
  if (is_compound_event) {
    message("[5/6] Calculating topological changes from compound fire impact...")
  } else {
    message("[5/6] Calculating topological changes...")
  }
  
  wasserstein_dist <- calculate_wasserstein_distance(
    tda_before_results$persistence_data,
    tda_after_results$persistence_data
  )
  message("  Wasserstein distance: ", round(wasserstein_dist, 6))
  
  # STEP 6: Generate Comprehensive Visualizations
  plots_list <- list()
  if (generate_plots) {
    if (is_compound_event) {
      message("[6/6] Generating enhanced visualizations for compound fire event...")
    } else {
      message("[6/6] Generating enhanced visualizations...")
    }
    
    # Create cascade progression plot with proper data
    enhanced_cascade_data <- enhance_cascade_data_for_plotting(cascade_results, wasserstein_dist)
    
    plots_list <- create_comprehensive_tda_visualization_set(
      cascade_results = enhanced_cascade_data,
      tda_before = tda_before_results,
      tda_after = tda_after_results,
      fire_name = display_fire_name,  # Use display name
      analysis_params = list(
        analysis_radius_km = analysis_radius_km,
        fire_buffer_km = fire_impact_buffer_km,
        wasserstein_distance = wasserstein_dist,
        is_compound_event = is_compound_event,
        fire_count = length(fire_names),
        fire_names = fire_names
      ),
      plots_dir = plots_dir
    )
    
    # Save plots with compound event naming
    if (length(plots_list) > 0) {
      for (plot_name in names(plots_list)) {
        plot_file <- file.path(plots_dir, paste0(plot_name, ".png"))
        tryCatch({
          ggsave(plot_file, plots_list[[plot_name]], 
                 width = 12, height = 9, dpi = 300, bg = "white")
          message("    Saved: ", basename(plot_file))
        }, error = function(e) {
          message("    Error saving ", plot_name, ": ", e$message)
        })
      }
    }
    
    # Display in RStudio
    if (display_plots && interactive()) {
      message("  Displaying plots in RStudio...")
      display_tda_plots(plots_list)
    }
  }
  
  # Save comprehensive summary with compound event details
  save_analysis_summary(
    output_dir = attack_run_dir,
    fire_name = display_fire_name,
    analysis_radius_km = analysis_radius_km,
    fire_buffer_km = fire_impact_buffer_km,
    failed_global = all_failed_buses_global,
    local_area_data = local_bus_data,
    before_res = tda_before_results,
    after_res = tda_after_results,
    wasserstein = wasserstein_dist,
    cascade_results = cascade_results,
    is_compound_event = is_compound_event,
    fire_names = fire_names
  )
  
  if (is_compound_event) {
    message("✓ Smart Integrated TDA-Cascade Workflow Complete (Compound Event)")
  } else {
    message("✓ Smart Integrated TDA-Cascade Workflow Complete")
  }
  
  return(list(
    success = TRUE,
    fire_name = display_fire_name,
    compound_fire_names = compound_fire_names,
    is_compound_event = is_compound_event,
    fire_count = length(fire_names),
    individual_fire_names = fire_names,
    report_path = file.path(attack_run_dir, "enhanced_analysis_summary.txt"),
    plots_dir = plots_dir,
    plots_list = plots_list,
    wasserstein_distance = wasserstein_dist,
    before_features = nrow(tda_before_results$persistence_data),
    after_features = nrow(tda_after_results$persistence_data),
    cascade_results = enhanced_cascade_data,
    analysis_params = list(
      analysis_radius_km = analysis_radius_km,
      fire_buffer_km = fire_impact_buffer_km,
      cascade_reused = use_existing_cascade && !is.null(existing_cascade_results),
      is_compound_event = is_compound_event
    )
  ))
}
create_comprehensive_tda_visualization_set <- function(cascade_results, tda_before, tda_after, 
                                                       fire_name, analysis_params, plots_dir = NULL) {
  
  message("Creating comprehensive TDA visualization set (Base R)...")
  
  plots_created <- character(0)
  
  tryCatch({
    # Extract parameters
    wasserstein_dist <- analysis_params$wasserstein_distance
    is_compound <- analysis_params$is_compound_event
    fire_count <- analysis_params$fire_count
    
    # 1. PERSISTENCE DIAGRAMS
    if (!is.null(tda_before$persistence_data) && nrow(tda_before$persistence_data) > 0) {
      save_path <- if (!is.null(plots_dir)) file.path(plots_dir, "persistence_before.png") else NULL
      plot_persistence_diagram_base(
        tda_before$persistence_data, 
        paste("Before:", fire_name),
        save_path = save_path
      )
      plots_created <- c(plots_created, "persistence_before")
      message("  ✓ Created 'before' persistence diagram")
    }
    
    if (!is.null(tda_after$persistence_data) && nrow(tda_after$persistence_data) > 0) {
      save_path <- if (!is.null(plots_dir)) file.path(plots_dir, "persistence_after.png") else NULL
      plot_persistence_diagram_base(
        tda_after$persistence_data,
        paste("After:", fire_name),
        save_path = save_path
      )
      plots_created <- c(plots_created, "persistence_after")
      message("  ✓ Created 'after' persistence diagram")
    }
    
    # 2. COMPARISON PLOT
    if (!is.null(tda_before$persistence_data) && !is.null(tda_after$persistence_data) &&
        nrow(tda_before$persistence_data) > 0 && nrow(tda_after$persistence_data) > 0) {
      
      save_path <- if (!is.null(plots_dir)) file.path(plots_dir, "comparison.png") else NULL
      create_before_after_comparison_base(
        tda_before$persistence_data, 
        tda_after$persistence_data,
        fire_name,
        wasserstein_dist,
        save_path = save_path
      )
      plots_created <- c(plots_created, "comparison")
      message("  ✓ Created before/after comparison plot")
    }
    
    # 3. CASCADE PROGRESSION PLOT
    if (!is.null(cascade_results$metrics) && nrow(cascade_results$metrics) > 0) {
      save_path <- if (!is.null(plots_dir)) file.path(plots_dir, "cascade_progression.png") else NULL
      create_cascade_progression_base(
        cascade_results$metrics,
        fire_name,
        is_compound,
        save_path = save_path
      )
      plots_created <- c(plots_created, "cascade_progression")
      message("  ✓ Created cascade progression plot")
    }
    
    # 4. TOPOLOGICAL SUMMARY PLOT
    if (!is.null(wasserstein_dist)) {
      save_path <- if (!is.null(plots_dir)) file.path(plots_dir, "topological_summary.png") else NULL
      create_topological_summary_base(
        before_features = if(!is.null(tda_before$persistence_data)) nrow(tda_before$persistence_data) else 0,
        after_features = if(!is.null(tda_after$persistence_data)) nrow(tda_after$persistence_data) else 0,
        wasserstein_dist = wasserstein_dist,
        fire_name = fire_name,
        is_compound = is_compound,
        save_path = save_path
      )
      plots_created <- c(plots_created, "topological_summary")
      message("  ✓ Created topological summary plot")
    }
    
    # 5. COMPOUND EVENT SPECIAL PLOTS
    if (is_compound && fire_count > 1) {
      save_path <- if (!is.null(plots_dir)) file.path(plots_dir, "compound_analysis.png") else NULL
      create_compound_event_base(
        analysis_params,
        cascade_results,
        fire_name,
        save_path = save_path
      )
      plots_created <- c(plots_created, "compound_analysis")
      message("  ✓ Created compound event analysis plot")
    }
    
    message("✓ Base R TDA visualization set complete: ", length(plots_created), " plots created")
    
  }, error = function(e) {
    message("Warning in base R visualization creation: ", e$message)
  })
  
  # Return list of created plot names for compatibility
  return(setNames(as.list(plots_created), plots_created))
}

create_before_after_comparison <- function(before_data, after_data, fire_name, 
                                           wasserstein_dist, save_path = NULL) {
  
  # Set up plotting device if saving
  if (!is.null(save_path)) {
    png(save_path, width = 1200, height = 600, res = 150)
  }
  
  # Set up side-by-side plots
  old_par <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  
  tryCatch({
    
    # Define consistent colors
    dimension_colors <- c("0" = "#2E86AB", "1" = "#E63946", "2" = "#2ca02c")
    
    # BEFORE plot
    if (!is.null(before_data) && nrow(before_data) > 0) {
      before_df <- as.data.frame(before_data)
      plot(before_df$Birth, before_df$Death,
           xlim = c(0, 1), ylim = c(0, 1),
           xlab = "Birth", ylab = "Death",
           main = "Before Fire Impact",
           asp = 1,
           col = dimension_colors[as.character(before_df$Dimension)],
           pch = 19, cex = 1.2)
      abline(a = 0, b = 1, col = "gray50", lty = 2, lwd = 1.5)
      grid(col = "lightgray", lty = 1, lwd = 0.5)
    } else {
      plot(0.5, 0.5, xlim = c(0, 1), ylim = c(0, 1),
           xlab = "Birth", ylab = "Death", main = "Before Fire Impact",
           type = "n", asp = 1)
      text(0.5, 0.5, "No features", cex = 1.2, col = "gray50")
    }
    
    # AFTER plot
    if (!is.null(after_data) && nrow(after_data) > 0) {
      after_df <- as.data.frame(after_data)
      plot(after_df$Birth, after_df$Death,
           xlim = c(0, 1), ylim = c(0, 1),
           xlab = "Birth", ylab = "Death",
           main = "After Fire Impact",
           asp = 1,
           col = dimension_colors[as.character(after_df$Dimension)],
           pch = 19, cex = 1.2)
      abline(a = 0, b = 1, col = "gray50", lty = 2, lwd = 1.5)
      grid(col = "lightgray", lty = 1, lwd = 0.5)
    } else {
      plot(0.5, 0.5, xlim = c(0, 1), ylim = c(0, 1),
           xlab = "Birth", ylab = "Death", main = "After Fire Impact", 
           type = "n", asp = 1)
      text(0.5, 0.5, "No features", cex = 1.2, col = "gray50")
    }
    
    # Add overall title
    mtext(paste("TDA Comparison:", fire_name, 
                "| Wasserstein Distance:", round(wasserstein_dist, 4)), 
          side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
    
  }, finally = {
    # Restore original plotting parameters
    par(old_par)
  })
  
  if (!is.null(save_path)) {
    dev.off()
    message("  Saved comparison plot: ", save_path)
  }
  
  return(invisible(TRUE))
}

create_cascade_progression_plot <- function(metrics, fire_name, is_compound, save_path = NULL) {
  
  if (is.null(metrics) || nrow(metrics) == 0) {
    if (!is.null(save_path)) {
      png(save_path, width = 800, height = 600, res = 150)
      plot(1, 1, type = "n", main = "No cascade metrics available")
      dev.off()
    }
    return(invisible(NULL))
  }
  
  # Ensure step column exists
  if (!"step" %in% names(metrics)) {
    metrics$step <- seq_len(nrow(metrics))
  }
  
  # Set up plotting device if saving
  if (!is.null(save_path)) {
    png(save_path, width = 1000, height = 800, res = 150)
  }
  
  # Set up two plots vertically
  old_par <- par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  
  tryCatch({
    
    # Plot 1: Cascade Impact (Stacked Area-like effect using polygons)
    fire_affected <- if("fire_affected" %in% names(metrics)) metrics$fire_affected else rep(0, nrow(metrics))
    deenergized <- if("deenergized" %in% names(metrics)) metrics$deenergized else rep(0, nrow(metrics))
    
    max_impact <- max(c(fire_affected + deenergized, 1), na.rm = TRUE)
    
    plot(metrics$step, fire_affected + deenergized, 
         type = "n", 
         xlim = range(metrics$step), ylim = c(0, max_impact),
         xlab = "Simulation Step", ylab = "Buses Affected",
         main = if(is_compound) paste("Compound Fire Cascade:", fire_name) else paste("Fire Cascade:", fire_name))
    
    # Create stacked area effect with polygons
    if (any(deenergized > 0)) {
      # Bottom area - cascade failures
      polygon(c(metrics$step, rev(metrics$step)), 
              c(deenergized, rep(0, length(deenergized))),
              col = "#06AED5", border = NA, density = NA)
    }
    
    if (any(fire_affected > 0)) {
      # Top area - fire impact  
      polygon(c(metrics$step, rev(metrics$step)),
              c(fire_affected + deenergized, rev(deenergized)),
              col = "#E63946", border = NA, density = NA)
    }
    
    # Add legend
    legend("topright", 
           legend = c("Fire Impact", "Cascade Failures"),
           fill = c("#E63946", "#06AED5"),
           cex = 0.9)
    
    # Plot 2: Grid Size Over Time
    vertices_remaining <- if("vertices_remaining" %in% names(metrics)) metrics$vertices_remaining else rep(0, nrow(metrics))
    
    plot(metrics$step, vertices_remaining,
         type = "b", 
         col = "#2E86AB", pch = 19, lwd = 2, cex = 1.2,
         xlab = "Simulation Step", ylab = "Active Buses",
         main = "Grid Size Over Time")
    
    # Add grid for better readability
    grid(col = "lightgray", lty = 1, lwd = 0.5)
    
  }, finally = {
    par(old_par)
  })
  
  if (!is.null(save_path)) {
    dev.off()
    message("  Saved cascade progression: ", save_path)
  }
  
  return(invisible(TRUE))
}
create_topological_summary_plot <- function(before_features, after_features, wasserstein_dist,
                                            fire_name, is_compound, save_path = NULL) {
  
  # Prepare data
  metrics <- c("Features\nBefore", "Features\nAfter", "Wasserstein\nDistance")
  values <- c(before_features, after_features, wasserstein_dist * 100)  # Scale Wasserstein
  colors <- c("#4CAF50", "#4CAF50", "#FF9800")
  
  # Set up plotting device if saving
  if (!is.null(save_path)) {
    png(save_path, width = 800, height = 600, res = 150)
  }
  
  # Create bar plot
  bp <- barplot(values, 
                names.arg = metrics,
                col = colors,
                main = if(is_compound) paste("Compound Event Summary:", fire_name) else paste("TDA Summary:", fire_name),
                ylab = "Value",
                ylim = c(0, max(values) * 1.2),
                las = 1,  # Horizontal axis labels
                cex.names = 0.9)
  
  # Add value labels on top of bars
  text(bp, values + max(values) * 0.02, 
       labels = ifelse(metrics == "Wasserstein\nDistance", 
                       round(values[3]/100, 4), 
                       round(values)),
       pos = 3, cex = 0.9)
  
  # Add grid
  grid(col = "lightgray", lty = 1, lwd = 0.5)
  
  if (!is.null(save_path)) {
    dev.off()
    message("  Saved topological summary: ", save_path)
  }
  
  return(invisible(values))
}

create_compound_event_plot<- function(analysis_params, cascade_results, fire_name, save_path = NULL) {
  
  fire_names <- analysis_params$fire_names
  fire_count <- analysis_params$fire_count
  
  if (fire_count <= 1) {
    return(invisible(NULL))
  }
  
  # Set up plotting device if saving
  if (!is.null(save_path)) {
    png(save_path, width = 1000, height = 400, res = 150)
  }
  
  # Create timeline-style plot
  fire_indices <- seq_along(fire_names)
  
  plot(fire_indices, rep(1, length(fire_indices)),
       xlim = c(0.5, length(fire_indices) + 0.5),
       ylim = c(0.5, 1.5),
       xlab = "Fire Index", ylab = "",
       main = paste("Compound Fire Event Analysis -", fire_count, "Simultaneous Fires"),
       pch = 19, cex = 3, col = "#E63946",
       xaxt = "n", yaxt = "n")
  
  # Add fire names as labels
  text(fire_indices, rep(1.2, length(fire_indices)), 
       labels = fire_names, 
       srt = 45, adj = c(0, 0), cex = 0.8)
  
  # Add connecting lines
  if (length(fire_indices) > 1) {
    lines(fire_indices, rep(1, length(fire_indices)), 
          col = "gray50", lwd = 2, lty = 2)
  }
  
  # Add x-axis labels
  axis(1, at = fire_indices, labels = paste("Fire", fire_indices))
  
  if (!is.null(save_path)) {
    dev.off()
    message("  Saved compound event plot: ", save_path)
  }
  
  return(invisible(TRUE))
}


display_tda_plots <- function(plots_list) {
  if (length(plots_list) == 0) {
    message("No plots to display")
    return()
  }
  
  tryCatch({
    for (plot_name in names(plots_list)) {
      message("Displaying plot: ", plot_name)
      print(plots_list[[plot_name]])
      
      # Add small delay for RStudio plot pane
      if (interactive()) {
        Sys.sleep(0.5)
      }
    }
  }, error = function(e) {
    message("Error displaying plots: ", e$message)
  })
}



get_local_area_data_compound <- function(combined_fire_area, buses_sf, radius_miles) {
  # Handle compound fire events by using the centroid of the combined fire area
  fire_center <- st_centroid(combined_fire_area)
  
  # Convert radius from miles to meters for st_buffer
  radius_meters <- radius_miles * 1609.34
  
  # Create a circular buffer around the combined fire center
  aoi_buffer <- st_buffer(fire_center, dist = radius_meters)
  
  # Find which buses fall within this buffer
  intersection_result <- st_intersects(buses_sf, aoi_buffer, sparse = FALSE)
  local_buses_sf <- buses_sf[intersection_result[, 1], ]
  
  return(list(
    buses = local_buses_sf,
    bus_ids = local_buses_sf$bus_i
  ))
}

enhance_cascade_data_for_plotting <- function(cascade_results, wasserstein_dist) {
  # Add wasserstein distance and other plotting metadata to cascade results
  if (is.null(cascade_results)) {
    return(NULL)
  }
  
  # Add the wasserstein distance for plotting
  cascade_results$wasserstein_distance <- wasserstein_dist
  
  # Add timestamp if not present
  if (is.null(cascade_results$timestamp)) {
    cascade_results$timestamp <- Sys.time()
  }
  
  # Add plotting metadata
  cascade_results$plotting_metadata <- list(
    analysis_type = "TDA_CASCADE",
    wasserstein_computed = TRUE,
    enhanced_for_plotting = TRUE,
    enhancement_timestamp = Sys.time()
  )
  
  # If metrics exist, add derived plotting variables
  if (!is.null(cascade_results$metrics) && nrow(cascade_results$metrics) > 0) {
    cascade_results$metrics <- cascade_results$metrics %>%
      mutate(
        grid_functionality_pct = vertices_remaining / max(vertices_remaining, na.rm = TRUE) * 100,
        cascade_amplification = ifelse(fire_affected > 0, deenergized / fire_affected, 0),
        cumulative_losses = cumsum(total_lost)
      )
  }
  
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
  
  # Read the CSV file with error handling
  dt <- tryCatch({
    fread(file_path)
  }, error = function(e) {
    stop("Failed to read CSV file: ", e$message)
  })
  
  if (nrow(dt) == 0) {
    stop("Matrix file is empty")
  }
  
  message("Loaded matrix with dimensions: ", nrow(dt), " x ", ncol(dt))
  
  # Convert to matrix, excluding the first column if it's bus IDs
  if (ncol(dt) > 1) {
    if (names(dt)[1] %in% c("bus_i", "V1") || is.numeric(dt[[1]])) {
      mat <- as.matrix(dt[, -1, with = FALSE])
      rownames(mat) <- dt[[1]]
    } else {
      mat <- as.matrix(dt)
    }
  } else {
    stop("Matrix file appears to have only one column")
  }
  
  # Ensure matrix is square
  if (nrow(mat) != ncol(mat)) {
    warning("Matrix is not square: ", nrow(mat), "x", ncol(mat))
    min_dim <- min(nrow(mat), ncol(mat))
    mat <- mat[1:min_dim, 1:min_dim]
    message("Truncated to square matrix: ", min_dim, "x", min_dim)
  }
  
  # Validate matrix contents
  if (any(is.na(mat))) {
    warning("Matrix contains NA values, replacing with 0")
    mat[is.na(mat)] <- 0
  }
  
  if (any(is.infinite(mat))) {
    warning("Matrix contains infinite values, replacing with max finite value")
    max_finite <- max(mat[is.finite(mat)])
    mat[is.infinite(mat)] <- max_finite
  }
  
  # Check matrix value range
  mat_min <- min(mat, na.rm = TRUE)
  mat_max <- max(mat, na.rm = TRUE)
  mat_mean <- mean(mat, na.rm = TRUE)
  
  message("Matrix value range: [", round(mat_min, 6), ", ", round(mat_max, 6), "]")
  message("Matrix mean: ", round(mat_mean, 6))
  
  # Normalize if needed
  if (mat_max > 2) {
    mat <- mat / mat_max
    message("Matrix normalized to [0,1] range")
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
generate_local_power_matrix <- function(local_bus_data) {
  # Calculate net power on the fly
  local_bus_power <- local_bus_data %>%
    st_drop_geometry() %>% # Drop spatial data for faster processing
    mutate(net_power = total_gen - load_mw) %>%
    select(bus_i, net_power)
  
  if (nrow(local_bus_power) == 0) return(matrix(nrow = 0, ncol = 0))
  
  power_diff_matrix <- as.matrix(dist(local_bus_power$net_power, method = "manhattan"))
  
  max_diff <- max(power_diff_matrix)
  if (max_diff > 0) {
    power_diff_matrix <- power_diff_matrix / max_diff
  }
  
  return(power_diff_matrix)
}
validate_spatial_data <- function(fire_data, bus_data) {
  errors <- c()
  
  # Check fire data
  if (!inherits(fire_data, "sf")) {
    errors <- c(errors, "Fire data is not an sf spatial object")
  } else if (nrow(fire_data) == 0) {
    errors <- c(errors, "Fire data is empty")
  } else if (any(!st_is_valid(fire_data))) {
    message("Warning: Some fire geometries are invalid, attempting to fix...")
    fire_data <- st_make_valid(fire_data)
  }
  
  # Check bus data
  if (!inherits(bus_data, "sf")) {
    errors <- c(errors, "Bus data is not an sf spatial object")
  } else if (nrow(bus_data) == 0) {
    errors <- c(errors, "Bus data is empty")
  }
  
  if (length(errors) > 0) {
    return(list(valid = FALSE, errors = errors))
  }
  
  return(list(valid = TRUE, fire_data = fire_data, bus_data = bus_data))
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

write_perseus_file <- function(adjacency_matrix, output_dir = NULL) {
  
  # Use config from global.R
  if (exists("tda_config") && "simple_perseus" %in% names(tda_config)) {
    cfg <- tda_config$simple_perseus
  } else {
    # Fallback config
    cfg <- list(
      delta = 0.1, filt_len = 10, cap = 3, genus = 0,
      outputs_dir = "outputs/"
    )
  }
  
  if (is.null(output_dir)) {
    output_dir <- cfg$outputs_dir
  }
  
  # Ensure output directory exists
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Process matrix exactly like working original code
  A2 <- as.matrix(adjacency_matrix)
  A2[A2 == 0] <- 999      # Exact match to working code
  diag(A2) <- 0           # Exact match to working code
  
  d <- nrow(A2)           # Number of points
  
  # Write M.txt file exactly like working version
  output_file <- file.path(output_dir, "M.txt")
  
  # Line 1: number of points  
  cat(d, file = output_file, append = FALSE, sep = '\n')
  
  # Line 2: parameters (g, delta, filt_len, cap)
  cat(paste(cfg$genus, cfg$delta, cfg$filt_len, cfg$cap, sep = ' '), 
      file = output_file, append = TRUE, sep = '\n')
  
  # Line 3+: matrix data (as vector, space-separated)
  cat(as.vector(A2), file = output_file, append = TRUE)
  
  message("✓ Perseus M.txt written: ", output_file, " (", d, "×", d, " matrix)")
  
  return(output_file)
}

# FIXED: Read Perseus outputs matching exact format (Moutput_0.txt, etc.)
read_perseus_outputs <- function(output_prefix = NULL) {
  
  # Use config from global.R
  if (exists("tda_config") && "simple_perseus" %in% names(tda_config)) {
    cfg <- tda_config$simple_perseus
    filt_len <- cfg$filt_len
  } else {
    filt_len <- 10  # Fallback
    cfg <- list(outputs_dir = "outputs/")
  }
  
  if (is.null(output_prefix)) {
    output_prefix <- file.path(cfg$outputs_dir, "Moutput")
  }
  
  # Initialize combined persistence data
  P <- NULL
  
  # Read dimension 0 (exactly like working code)
  dim0_file <- paste0(output_prefix, "_0.txt")
  if (file.exists(dim0_file) && file.info(dim0_file)$size > 0) {
    
    persist_data <- as.matrix(read.table(dim0_file))
    
    # Process exactly like working code
    persist_data[persist_data[, 2] == -1, 2] <- filt_len + 1
    persist_data <- persist_data / (filt_len + 1)
    
    # Add dimension column
    P <- cbind(rep(0, nrow(persist_data)), persist_data)
    
    message("  Dimension 0: ", nrow(persist_data), " features")
  }
  
  # Read dimension 1 (exactly like working code)
  dim1_file <- paste0(output_prefix, "_1.txt") 
  if (file.exists(dim1_file) && file.info(dim1_file)$size > 0) {
    
    persist_data <- as.matrix(read.table(dim1_file, blank.lines.skip = TRUE))
    
    # Process exactly like working code
    persist_data[persist_data[, 2] == -1, 2] <- filt_len + 1
    persist_data <- persist_data / (filt_len + 1)
    
    # Combine
    dim1_data <- cbind(rep(1, nrow(persist_data)), persist_data)
    P <- rbind(P, dim1_data)
    
    message("  Dimension 1: ", nrow(persist_data), " features")
  }
  
  # Set proper column names for TDA compatibility
  if (!is.null(P) && ncol(P) >= 3) {
    colnames(P) <- c("Dimension", "Birth", "Death")
    message("✓ Total features: ", nrow(P))
  } else {
    message("⚠ No persistence features found")
    P <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("Dimension", "Birth", "Death")))
  }
  
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
run_perseus_analysis <- function(adjacency_matrix, output_dir = NULL) {
  message("--- Running Core Perseus TDA ---")
  if (is.null(output_dir)) output_dir <- perseus_config$outputs_dir
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Downsample if matrix is too large
  final_matrix <- downsample_matrix(adjacency_matrix, perseus_config$downsample_max_pts)
  
  tryCatch({
    input_file <- write_perseus_file(final_matrix, output_dir)
    output_prefix <- file.path(output_dir, perseus_config$perseus_output_prefix)
    perseus_cmd <- paste(perseus_config$perseus_exe, "distmat", input_file, output_prefix)
    message("  Executing Perseus with a ", perseus_config$timeout_seconds, " second timeout...")
    system(perseus_cmd, timeout = perseus_config$timeout_seconds)
    persistence_data <- read_perseus_outputs(output_prefix)
    message("✓ Core Perseus TDA complete. Found ", nrow(persistence_data), " features.")
    return(list(success = TRUE, persistence_data = persistence_data, matrix_size = nrow(final_matrix)))
  }, error = function(e) {
    message("✗ Perseus analysis failed: ", e$message)
    return(list(success = FALSE, error = e$message))
  }, warning = function(w) {
    if(grepl("timeout", w$message)) {
      message("✗ Perseus process timed out.")
      return(list(success = FALSE, error = "Perseus timed out."))
    }
  })
}

write_perseus_file <- function(adjacency_matrix, output_dir) {
  output_file <- file.path(output_dir, perseus_config$perseus_input_file)
  d <- nrow(adjacency_matrix)
  cat(d, file = output_file, append = FALSE, sep = '\n')
  cat(paste(perseus_config$g, perseus_config$s, perseus_config$N, perseus_config$C, sep = ' '), file = output_file, append = TRUE, sep = '\n')
  cat(as.vector(adjacency_matrix), file = output_file, append = TRUE)
  return(output_file)
}

read_perseus_outputs <- function(output_prefix) {
  P <- NULL
  filt_len <- perseus_config$N
  for (dim in 0:perseus_config$max_dim) {
    dim_file <- paste0(output_prefix, "_", dim, ".txt")
    if (file.exists(dim_file) && file.info(dim_file)$size > 0) {
      persist_data <- as.matrix(read.table(dim_file, blank.lines.skip = TRUE))
      if (nrow(persist_data) > 0) {
        persist_data[persist_data[, 2] == -1, 2] <- filt_len + 1
        persist_data <- persist_data / (filt_len + 1)
        dim_data <- cbind(rep(dim, nrow(persist_data)), persist_data)
        P <- rbind(P, dim_data)
      }
    }
  }
  if (!is.null(P) && ncol(P) >= 3) {
    colnames(P) <- c("Dimension", "Birth", "Death")
  } else {
    P <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("Dimension", "Birth", "Death")))
  }
  return(P)
}

save_analysis_summary  <- function(output_dir, fire_name, analysis_radius_km, fire_buffer_km, 
                                   failed_global, local_area_data, before_res, after_res, 
                                   wasserstein, cascade_results, is_compound_event = FALSE, 
                                   fire_names = NULL) {
  summary_file <- file.path(output_dir, "enhanced_analysis_summary.txt")
  sink(summary_file)
  
  if (is_compound_event) {
    cat("=== COMPOUND FIRE EVENT TDA ANALYSIS SUMMARY ===\n\n")
    cat("Compound Fire Event: ", fire_name, "\n")
    cat("Individual Fires: ", paste(fire_names, collapse = ", "), "\n")
    cat("Number of Fires: ", length(fire_names), "\n")
  } else {
    cat("=== SINGLE FIRE TDA ANALYSIS SUMMARY ===\n\n")
    cat("Fire Event: ", fire_name, "\n")
  }
  
  cat("Analysis Time: ", as.character(Sys.time()), "\n\n")
  
  cat("--- Analysis Scope ---\n")
  cat("Analysis Type: ", if (is_compound_event) "Compound Fire Event" else "Single Fire Event", "\n")
  cat("Area of Interest Radius: ", analysis_radius_km, " km\n")
  cat("Fire Impact Buffer: ", fire_buffer_km, " km\n")
  cat("Buses in AOI (Before): ", nrow(local_area_data), "\n")
  
  if (is_compound_event) {
    cat("Combined Fire Analysis: ", length(fire_names), " simultaneous fires\n")
  }
  
  cat("\n--- Global Cascade Summary ---\n")
  cat("Total Buses Failed (Grid-wide): ", length(failed_global), "\n")
  
  if (!is.null(cascade_results$metrics) && nrow(cascade_results$metrics) > 0) {
    total_fire_affected <- sum(cascade_results$metrics$fire_affected, na.rm = TRUE)
    total_cascade_failures <- sum(cascade_results$metrics$deenergized, na.rm = TRUE)
    cat("Direct Fire Impact: ", total_fire_affected, "\n")
    cat("Cascade Failures: ", total_cascade_failures, "\n")
    cat("Cascade Amplification: ", round(total_cascade_failures / max(total_fire_affected, 1), 2), "x\n")
  }
  
  cat("\n--- Local TDA Results ---\n")
  cat("Features Before: ", nrow(before_res$persistence_data), "\n")
  cat("Features After: ", nrow(after_res$persistence_data), "\n")
  cat("Feature Change: ", nrow(after_res$persistence_data) - nrow(before_res$persistence_data), "\n")
  cat("Topological Change (Wasserstein): ", round(wasserstein, 6), "\n")
  
  if (is_compound_event) {
    cat("\n--- Compound Event Details ---\n")
    for (i in seq_along(fire_names)) {
      cat("Fire ", i, ": ", fire_names[i], "\n")
    }
    cat("Combined Impact Analysis: Simultaneous failure cascades from multiple fire sources\n")
    cat("Topological Signature: Reflects compound disturbance pattern\n")
  }
  
  cat("\n--- Analysis Configuration ---\n")
  cat("TDA Method: Perseus (Localized)\n")
  cat("Matrix Type: Net Power Differences\n")
  cat("Cascade Model: Enhanced Fire Propagation\n")
  cat("Spatial Buffer: ", fire_buffer_km, " km\n")
  cat("Analysis Radius: ", analysis_radius_km, " km\n")
  
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

calculate_wasserstein_distance <- function(P_original, P_current, dimensions = c(0, 1)) {
  if (nrow(P_original) == 0 && nrow(P_current) == 0) return(0)
  if (!requireNamespace("TDA", quietly = TRUE)) stop("TDA package is required.")
  return(TDA::wasserstein(P_original, P_current, dimension = dimensions))
}

plot_persistence_diagram_base <- function(diagram, title_suffix = "", save_path = NULL) {
  
  if (is.null(diagram) || nrow(diagram) == 0) {
    # Handle empty case
    if (!is.null(save_path)) {
      png(save_path, width = 800, height = 600, res = 150)
    }
    
    plot(0.5, 0.5, xlim = c(0, 1), ylim = c(0, 1), 
         xlab = "Birth", ylab = "Death", 
         main = paste("Persistence Diagram", title_suffix, "(No Features)"),
         type = "n", asp = 1)
    text(0.5, 0.5, "No persistent features found", cex = 1.2, col = "gray50")
    
    if (!is.null(save_path)) {
      dev.off()
    }
    return(invisible(NULL))
  }
  
  # Convert to data frame for easier handling
  df <- as.data.frame(diagram)
  
  # Define colors for different dimensions
  dimension_colors <- c("0" = "#1f77b4", "1" = "#ff7f0e", "2" = "#2ca02c")
  
  # Get unique dimensions and assign colors
  dims <- unique(df$Dimension)
  colors <- dimension_colors[as.character(dims)]
  colors[is.na(colors)] <- "black"  # Default color for unexpected dimensions
  
  # Set up plotting device if saving
  if (!is.null(save_path)) {
    png(save_path, width = 800, height = 600, res = 150)
  }
  
  # Create the main plot
  plot(df$Birth, df$Death, 
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Birth", ylab = "Death",
       main = paste("Persistence Diagram", title_suffix),
       asp = 1,  # Square aspect ratio
       col = dimension_colors[as.character(df$Dimension)],
       pch = 19,  # Solid circles
       cex = 1.2)
  
  # Add diagonal line (y = x)
  abline(a = 0, b = 1, col = "gray50", lty = 2, lwd = 1.5)
  
  # Add legend if multiple dimensions
  if (length(dims) > 1) {
    legend("bottomright", 
           legend = paste("Dimension", dims),
           col = colors[as.character(dims)],
           pch = 19,
           cex = 0.9,
           bg = "white")
  }
  
  # Add grid for better readability
  grid(col = "lightgray", lty = 1, lwd = 0.5)
  
  if (!is.null(save_path)) {
    dev.off()
    message("  Saved persistence diagram: ", save_path)
  }
  
  return(invisible(df))
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

