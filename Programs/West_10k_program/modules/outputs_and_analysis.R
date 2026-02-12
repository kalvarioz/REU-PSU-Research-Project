# =================================================================================================
# modules/outputs_and_analysis.R
# 
# Output Rendering and Analysis Execution Functions
#
# This module contains functions for:
# - TDA comparison plots
# - Cascade progression plots
# - System status outputs
# - Resilience plots
# - Fire timeline plots
# - Damage assessment tables (bus type breakdown, risk sorting)
# - Download handlers (results, plots, GIS data, matrices)
#
# Brandon Calvario
# =================================================================================================

# Helper: Create archive with fallback for systems without zip
safe_archive <- function(archive_file, files_to_archive) {
  zip_ok <- tryCatch({
    zip(archive_file, files_to_archive, flags = "-j")
    TRUE
  }, error = function(e) FALSE,
  warning = function(w) file.exists(archive_file))
  
  if (!zip_ok || !file.exists(archive_file)) {
    tryCatch({
      tar_file <- sub("\\.zip$", ".tar.gz", archive_file)
      tar(tar_file, files_to_archive, compression = "gzip")
      if (file.exists(tar_file)) file.copy(tar_file, archive_file, overwrite = TRUE)
    }, error = function(e) {
      if (length(files_to_archive) > 0 && file.exists(files_to_archive[1]))
        file.copy(files_to_archive[1], archive_file, overwrite = TRUE)
    })
  }
}

# Helper: Compute damage assessment data from cascade results
compute_damage_assessment <- function(cascade_results, step = NULL) {
  
  message("=== DAMAGE ASSESSMENT COMPUTATION ===")
  
  # Step 1: Determine step and gather lost buses
  if (is.null(step) || step <= 0) step <- length(cascade_results$graphs) - 1
  n_steps <- length(cascade_results$buses_lost_per_step)
  message("  Requested step: ", step, " | Available steps: ", n_steps)
  
  if (n_steps == 0) {
    message("  WARNING: buses_lost_per_step is empty")
    return(list(bus_damage = data.frame(), branch_damage = data.frame(),
                type_summary = data.frame(), total_gen_lost_mw = 0, 
                total_load_lost_mw = 0, n_lost = 0))
  }
  
  step <- min(step, n_steps)
  all_lost_buses <- unique(unlist(cascade_results$buses_lost_per_step[1:step]))
  message("  Total lost buses up to step ", step, ": ", length(all_lost_buses))
  
  if (length(all_lost_buses) == 0) {
    message("  No buses lost - returning empty result")
    return(list(bus_damage = data.frame(), branch_damage = data.frame(),
                type_summary = data.frame(), total_gen_lost_mw = 0, 
                total_load_lost_mw = 0, n_lost = 0))
  }
  
  # Step 2: Get bus details
  if (!exists("bus_info") || nrow(bus_info) == 0) {
    message("  ERROR: bus_info not available")
    return(NULL)
  }
  
  lost_bus_data <- tryCatch({
    df <- bus_info %>% sf::st_drop_geometry() %>% dplyr::filter(bus_i %in% all_lost_buses)
    message("  Matched buses in bus_info: ", nrow(df))
    df
  }, error = function(e) {
    message("  ERROR filtering bus_info: ", e$message)
    return(NULL)
  })
  
  if (is.null(lost_bus_data) || nrow(lost_bus_data) == 0) {
    message("  WARNING: No matching buses found in bus_info")
    return(list(bus_damage = data.frame(), branch_damage = data.frame(),
                type_summary = data.frame(), total_gen_lost_mw = 0, 
                total_load_lost_mw = 0, n_lost = 0))
  }
  
  # Step 3: Add power impact and severity (safe - no complex sapply)
  lost_bus_data <- lost_bus_data %>%
    dplyr::mutate(
      power_impact_mw = total_gen + load_mw,
      severity = dplyr::case_when(
        bus_type == "Gen + Load" ~ "Critical",
        bus_type == "Generator" ~ "High",
        bus_type == "Load" ~ "Moderate",
        TRUE ~ "Low"
      )
    )
  
  # Step 4: Determine failure cause (isolated from main pipeline)
  failure_causes <- tryCatch({
    causes <- rep("Cascade Failure", nrow(lost_bus_data))
    for (i in seq_len(nrow(lost_bus_data))) {
      b <- lost_bus_data$bus_i[i]
      for (s in seq_len(step)) {
        if (b %in% cascade_results$buses_lost_per_step[[s]]) {
          # Check fire_points_list safely
          fp <- cascade_results$fire_points_list[[s]]
          if (!is.null(fp) && nrow(fp) > 0) {
            fp_bus_ids <- tryCatch(fp$bus_i, error = function(e) integer(0))
            if (b %in% fp_bus_ids) {
              impact <- tryCatch({
                idx <- which(fp_bus_ids == b)[1]
                as.character(fp$impact_type[idx])
              }, error = function(e) "fire")
              causes[i] <- paste0("Fire (", tools::toTitleCase(impact), ") - Step ", s)
            } else {
              causes[i] <- paste0("Cascade - Step ", s)
            }
          } else {
            causes[i] <- paste0("Cascade - Step ", s)
          }
          break  # Found the step, stop looking
        }
      }
    }
    causes
  }, error = function(e) {
    message("  WARNING: Could not determine failure causes: ", e$message)
    rep("Unknown", nrow(lost_bus_data))
  })
  
  lost_bus_data$failure_cause <- failure_causes
  lost_bus_data <- lost_bus_data %>% dplyr::arrange(dplyr::desc(power_impact_mw))
  
  # Step 5: Build bus damage table
  bus_damage <- tryCatch({
    # Safely select columns (baseKV might not exist)
    cols_available <- names(lost_bus_data)
    base_kv_col <- if ("baseKV" %in% cols_available) lost_bus_data$baseKV 
    else if ("base_kv" %in% cols_available) lost_bus_data$base_kv
    else rep(NA, nrow(lost_bus_data))
    
    data.frame(
      Bus = lost_bus_data$bus_i,
      Type = lost_bus_data$bus_type,
      Severity = lost_bus_data$severity,
      `Gen (MW)` = round(lost_bus_data$total_gen, 1),
      `Load (MW)` = round(lost_bus_data$load_mw, 1),
      `Total Impact (MW)` = round(lost_bus_data$power_impact_mw, 1),
      `Base kV` = round(base_kv_col, 1),
      Cause = lost_bus_data$failure_cause,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("  ERROR building bus_damage table: ", e$message)
    data.frame()
  })
  
  message("  Bus damage table: ", nrow(bus_damage), " rows")
  
  # Step 6: Build type summary
  type_summary <- tryCatch({
    lost_bus_data %>%
      dplyr::group_by(bus_type) %>%
      dplyr::summarise(
        count = dplyr::n(),
        gen_lost_mw = sum(total_gen, na.rm = TRUE),
        load_lost_mw = sum(load_mw, na.rm = TRUE),
        total_impact_mw = sum(power_impact_mw, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(total_impact_mw)) %>%
      dplyr::rename(
        `Bus Type` = bus_type, `Count` = count,
        `Gen Lost (MW)` = gen_lost_mw, `Load Lost (MW)` = load_lost_mw,
        `Total Impact (MW)` = total_impact_mw
      )
  }, error = function(e) {
    message("  ERROR building type_summary: ", e$message)
    data.frame()
  })
  
  message("  Type summary: ", nrow(type_summary), " types")
  
  # Step 7: Branch damage (with column name detection)
  branch_damage <- tryCatch({
    if (!exists("branch_info") || nrow(branch_info) == 0) {
      message("  branch_info not available, skipping branch damage")
      return(data.frame())
    }
    
    bi_cols <- names(branch_info)
    message("  branch_info columns: ", paste(head(bi_cols, 15), collapse = ", "))
    
    # Detect the correct column names (handles left_join .x/.y suffixes)
    from_col <- if ("from_bus" %in% bi_cols) "from_bus"
    else if ("from_bus.x" %in% bi_cols) "from_bus.x"
    else if ("fbus" %in% bi_cols) "fbus"
    else NULL
    to_col   <- if ("to_bus" %in% bi_cols) "to_bus"
    else if ("to_bus.x" %in% bi_cols) "to_bus.x"
    else if ("tbus" %in% bi_cols) "tbus"
    else NULL
    rate_col <- if ("rateA" %in% bi_cols) "rateA"
    else if ("RateA" %in% bi_cols) "RateA"
    else NULL
    r_col    <- if ("r" %in% bi_cols) "r" else if ("LineR" %in% bi_cols) "LineR" else NULL
    x_col    <- if ("x" %in% bi_cols) "x" else if ("LineX" %in% bi_cols) "LineX" else NULL
    
    if (is.null(from_col) || is.null(to_col)) {
      message("  WARNING: Cannot find from/to bus columns in branch_info")
      message("  Available columns: ", paste(bi_cols, collapse = ", "))
      return(data.frame())
    }
    
    message("  Using columns: from=", from_col, " to=", to_col, " rate=", rate_col)
    
    from_buses <- branch_info[[from_col]]
    to_buses <- branch_info[[to_col]]
    
    affected_idx <- which(from_buses %in% all_lost_buses | to_buses %in% all_lost_buses)
    if (length(affected_idx) == 0) {
      message("  No affected branches found")
      return(data.frame())
    }
    
    # Build branch damage df manually for safety
    affected_df <- data.frame(
      From = from_buses[affected_idx],
      To = to_buses[affected_idx],
      stringsAsFactors = FALSE
    )
    
    # Add rate, r, x if available
    if (!is.null(rate_col)) affected_df$`Rate A (MVA)` <- branch_info[[rate_col]][affected_idx]
    if (!is.null(r_col)) affected_df$`R (p.u.)` <- branch_info[[r_col]][affected_idx]
    if (!is.null(x_col)) affected_df$`X (p.u.)` <- branch_info[[x_col]][affected_idx]
    
    # Add status
    affected_df$Status <- ifelse(
      affected_df$From %in% all_lost_buses & affected_df$To %in% all_lost_buses,
      "Destroyed", "Degraded"
    )
    
    # Look up bus types for from/to
    bus_type_lookup <- stats::setNames(
      as.character(bus_info$bus_type), 
      as.character(bus_info$bus_i)
    )
    affected_df$`From Type` <- bus_type_lookup[as.character(affected_df$From)]
    affected_df$`To Type` <- bus_type_lookup[as.character(affected_df$To)]
    affected_df$`From Type`[is.na(affected_df$`From Type`)] <- "Unknown"
    affected_df$`To Type`[is.na(affected_df$`To Type`)] <- "Unknown"
    
    # Sort by rate and limit
    if ("Rate A (MVA)" %in% names(affected_df)) {
      affected_df <- affected_df[order(-affected_df$`Rate A (MVA)`), ]
    }
    
    head(affected_df, 500)
  }, error = function(e) {
    message("  ERROR building branch_damage: ", e$message)
    data.frame()
  })
  
  message("  Branch damage: ", nrow(branch_damage), " rows")
  message("=== DAMAGE ASSESSMENT COMPLETE ===")
  
  return(list(
    bus_damage = bus_damage,
    branch_damage = branch_damage,
    type_summary = type_summary,
    total_gen_lost_mw = sum(lost_bus_data$total_gen, na.rm = TRUE),
    total_load_lost_mw = sum(lost_bus_data$load_mw, na.rm = TRUE),
    n_lost = length(all_lost_buses)
  ))
}

# ====================================================================
# OUTPUT FUNCTIONS AND ANALYSIS EXECUTION
# ====================================================================

render_output_functions <- function(output, values, input, selected_fire, selected_state = NULL) {
  output$tda_comparison_plot <- renderPlot({
    req(values$tda_results)
    
    tryCatch({
      if (!values$tda_results$success) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = paste("TDA Analysis Failed:", values$tda_results$error)) +
                 theme_minimal())
      }
      
      # Check if we have plots available from TDA workflow
      if (!is.null(values$tda_results$plots_list) && 
          "comparison" %in% names(values$tda_results$plots_list)) {
        
        comparison_plot <- values$tda_results$plots_list$comparison
        
        if (inherits(comparison_plot, "ggplot")) {
          return(comparison_plot)
        }
      }
      
      # Fallback: create enhanced comparison plot if data is available
      if (!is.null(values$tda_results$before_persistence_data) && 
          !is.null(values$tda_results$after_persistence_data)) {
        
        return(create_before_after_comparison(
          values$tda_results$before_persistence_data,
          values$tda_results$after_persistence_data,
          values$tda_results$fire_name,
          values$tda_results$wasserstein_distance
        ))
      }
      
      # Final fallback: simple comparison
      if (!is.null(values$tda_results$before_features) && 
          !is.null(values$tda_results$after_features)) {
        
        comparison_data <- data.frame(
          State = c("Before", "After"),
          Features = c(values$tda_results$before_features, 
                       values$tda_results$after_features)
        )
        
        ggplot(comparison_data, aes(x = State, y = Features, fill = State)) +
          geom_col(alpha = 0.8, width = 0.6) +
          scale_fill_manual(values = c("Before" = "#2E86AB", "After" = "#E63946")) +
          labs(
            title = "TDA Feature Comparison",
            subtitle = paste("Wasserstein Distance:", 
                             round(values$tda_results$wasserstein_distance, 4)),
            y = "Number of Features"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            legend.position = "none",
            plot.title = element_text(size = 14, face = "bold")
          )
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No TDA comparison data available") +
          theme_minimal()
      }
      
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error creating TDA plot:", e$message)) +
        theme_minimal()
    })
  })
  
  output$cascade_progression_plot <- renderPlot({
    req(values$enhanced_cascade_results)
    
    tryCatch({
      # First priority: Check if we have ggplot from TDA results
      if (!is.null(values$tda_results) && 
          !is.null(values$tda_results$plots_list) &&
          "cascade_progression" %in% names(values$tda_results$plots_list)) {
        
        cascade_plot <- values$tda_results$plots_list$cascade_progression
        
        if (inherits(cascade_plot, "ggplot")) {
          return(cascade_plot)
        }
      }
      
      # Fallback: create plot from cascade metrics using FIXED function
      metrics <- values$enhanced_cascade_results$metrics
      
      if (nrow(metrics) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No cascade data") +
                 theme_minimal())
      }
      
      # Get fire name and compound status
      fire_name <- if (!is.null(values$enhanced_cascade_results$fire_name)) {
        values$enhanced_cascade_results$fire_name
      } else {
        "Unknown Fire"
      }
      
      is_compound <- if (!is.null(values$enhanced_cascade_results$is_compound_event)) {
        values$enhanced_cascade_results$is_compound_event
      } else {
        FALSE
      }
      return(create_cascade_progression_plot(metrics, fire_name, is_compound))
      
    }, error = function(e) {
      message("Error creating cascade progression plot: ", e$message)
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Error creating cascade plot:", e$message)) +
               theme_minimal())
    })
  })
  
  # ====================================================================
  # EXISTING OUTPUTS
  # ====================================================================
  
  # Enhanced System Status Output with compound fire information
  output$system_status <- renderText({
    status_lines <- c(
      "=== Wildfire Grid Resilience System ===",
      paste("System Time:", Sys.time()),
      "",
      "--- Grid Information ---",
      paste("Total Buses:", nrow(bus_info)),
      paste("Total Branches:", nrow(branch_info)),
      paste("Generator Buses:", sum(bus_info$total_gen > 0)),
      paste("Load Buses:", sum(bus_info$load_mw > 0)),
      "",
      "--- Wildfire Data ---",
      paste("Fire Perimeters Loaded:", nrow(wfigs_perimeters)),
      paste("States with Fires:", length(unique(wfigs_perimeters$attr_POOState)))
    )
    
    # Enhanced selected fire info with compound fire detection
    tryCatch({
      fire_data <- selected_fire()
      if (!is.null(fire_data) && nrow(fire_data) > 0) {
        
        fire_names <- unique(fire_data$attr_IncidentName)
        is_compound <- length(fire_names) > 1
        
        if (is_compound) {
          status_lines <- c(status_lines,
                            "",
                            "--- Selected Compound Fire Event ---",
                            paste("Fire Events:", length(fire_names)),
                            paste("Fire Names:", paste(fire_names, collapse = ", ")),
                            paste("Event Type: COMPOUND FIRE ANALYSIS"),
                            paste("Combined Area:", round(sum(fire_data$fire_acres, na.rm = TRUE)), "acres"),
                            paste("Total Polygons:", nrow(fire_data)),
                            paste("Time Steps:", length(unique(fire_data$step))),
                            paste("Has Coordinates:", any(fire_data$has_center_point))
          )
        } else {
          status_lines <- c(status_lines,
                            "",
                            "--- Selected Fire ---",
                            paste("Fire Name:", fire_names[1]),
                            paste("Event Type: SINGLE FIRE ANALYSIS"),
                            paste("Intensity:", unique(fire_data$fire_intensity)[1]),
                            paste("Total Area:", round(sum(fire_data$fire_acres, na.rm = TRUE)), "acres"),
                            paste("Polygons:", nrow(fire_data)),
                            paste("Time Steps:", length(unique(fire_data$step))),
                            paste("Has Coordinates:", any(fire_data$has_center_point))
          )
        }
        
        # Enhanced TDA results with compound fire information
        if (!is.null(values$tda_results)) {
          tda <- values$tda_results
          status_lines <- c(status_lines,
                            "",
                            "--- COMPLETE TDA ANALYSIS ---")
          
          if (tda$success) {
            analysis_type <- if (!is.null(tda$is_compound_event) && tda$is_compound_event) {
              paste("COMPOUND EVENT (", tda$fire_count, " fires)")
            } else {
              "SINGLE FIRE"
            }
            
            status_lines <- c(status_lines,
                              paste("Analysis Type:", analysis_type),
                              paste("Fire(s) Analyzed:", tda$fire_name),
                              paste("Analysis Radius:", tda$analysis_params$analysis_radius_km, "km"),
                              paste("Status: COMPLETED SUCCESSFULLY"))
            
            if (!is.null(tda$plots_list)) {
              status_lines <- c(status_lines,
                                paste("ggplot2 Plots Generated:", length(tda$plots_list)),
                                paste("Plot Types:", paste(names(tda$plots_list), collapse = ", ")))
            }
            
            if (!is.null(tda$report_path)) {
              status_lines <- c(status_lines,
                                paste("Report Location:", basename(dirname(tda$report_path))))
            }
            
            if (!is.null(tda$before_features) && !is.null(tda$after_features)) {
              status_lines <- c(status_lines,
                                paste("Features Before:", tda$before_features),
                                paste("Features After:", tda$after_features),
                                paste("Wasserstein Distance:", round(tda$wasserstein_distance, 6)))
            }
            
            if (is_compound) {
              status_lines <- c(status_lines,
                                "Analysis includes: Compound fire topology comparison",
                                "Multi-fire cascade simulation completed",
                                "Combined topological impact assessed",
                                "Compound disturbance signature captured")
            } else {
              status_lines <- c(status_lines,
                                "Analysis includes: Before/After topology comparison",
                                "Localized area analysis completed",
                                "Full cascade simulation completed", 
                                "Wasserstein distance calculated")
            }
          } else {
            status_lines <- c(status_lines,
                              paste("Fire(s):", tda$fire_name),
                              paste("Status: FAILED"),
                              paste("Error:", tda$error))
          }
        }
      }
    }, error = function(e) {
      message("Error accessing selected fire: ", e$message)
    })
    # Enhanced simulation status with compound fire detection
    if (!is.null(values$enhanced_cascade_results)) {
      tryCatch({
        result <- values$enhanced_cascade_results
        current_step <- input$step
        # Check if this was a compound event
        is_cascade_compound <- !is.null(result$is_compound_event) && result$is_compound_event
        if (!is.null(current_step) && current_step <= nrow(result$metrics)) {
          step_metrics <- result$metrics[current_step, ]
          cascade_type <- if (is_cascade_compound) "COMPOUND FIRE CASCADE" else "FIRE CASCADE"
          status_lines <- c(status_lines,
                            "",
                            paste("--- ", cascade_type, " Step", current_step, "---"),
                            paste("Fire-Affected (Direct):", step_metrics$direct_hits),
                            paste("Fire-Affected (Buffer):", step_metrics$buffer_hits),
                            paste("Cascade Failures:", step_metrics$deenergized),
                            paste("Total Lost:", step_metrics$total_lost),
                            paste("Cascade Ratio:", round(step_metrics$cascade_ratio, 3)),
                            paste("Active Buses:", step_metrics$vertices_remaining),
                            paste("Active Lines:", step_metrics$edges_remaining),
                            paste("Grid Functionality:", 
                                  round(step_metrics$vertices_remaining/vcount(graph_original)*100, 1), "%")
          )
        }
        
        # Overall statistics with compound fire notation
        total_fire_affected <- sum(result$metrics$fire_affected)
        total_deenergized <- sum(result$metrics$deenergized)
        impact_type <- if (is_cascade_compound) "Compound Fire Impact" else "Fire Impact"
        status_lines <- c(status_lines,
                          "",
                          paste("--- Overall", impact_type, "---"),
                          paste("Total Fire-Affected:", total_fire_affected),
                          paste("Total Cascade Failures:", total_deenergized),
                          paste("Total Buses Lost:", total_fire_affected + total_deenergized),
                          paste("Final Grid Size:", tail(result$metrics$vertices_remaining, 1), "buses"),
                          paste("Simulation Steps:", length(result$graphs) - 1)
        )
        if (is_cascade_compound) {
          status_lines <- c(status_lines,
                            paste("Fire Events in Compound:", result$fire_count))
        }
        
      }, error = function(e) {
        message("Error processing cascade results: ", e$message)
      })
    }
    paste(status_lines, collapse = "\n")
  })
  
  # Resilience Plot
  output$resilience_plot <- renderPlot({
    req(values$cascade_results)
    tryCatch({
      metrics <- values$enhanced_cascade_results$metrics
      if (nrow(metrics) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No metrics available") +
                 theme_minimal())
      }
      # Ensure step column exists
      if (!"step" %in% names(metrics)) {
        metrics <- metrics %>% mutate(step = row_number())
      }
      metrics <- metrics %>%
        mutate(
          grid_functionality = vertices_remaining / vcount(graph_original) * 100,
          connectivity = edges_remaining / ecount(graph_original) * 100
        )
      metrics_long <- metrics %>%
        select(step, grid_functionality, connectivity) %>%
        pivot_longer(cols = -step, names_to = "metric", values_to = "value")
      ggplot(metrics_long, aes(x = step, y = value, color = metric)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c("grid_functionality" = "#2E86AB", 
                     "connectivity" = "#A23B72"),
          labels = c("Grid Functionality", "Network Connectivity")
        ) +
        labs(
          title = "Grid Resilience Over Time",
          x = "Simulation Step",
          y = "Percentage (%)",
          color = "Metric"
        ) +
        scale_y_continuous(limits = c(0, 100)) +
        theme_minimal() +
        theme(legend.position = "bottom")
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_minimal()
    })
  })
  
  # Cascade Plot
  output$cascade_plot <- renderPlot({
    req(values$enhanced_cascade_results)
    
    tryCatch({
      metrics <- values$enhanced_cascade_results$metrics
      
      if (nrow(metrics) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No cascade data") +
                 theme_minimal())
      }
      
      # Ensure step column and required columns exist
      if (!"step" %in% names(metrics)) {
        metrics <- metrics %>% mutate(step = row_number())
      }
      
      required_cols <- c("direct_hits", "buffer_hits", "deenergized")
      missing_cols <- required_cols[!required_cols %in% names(metrics)]
      
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          metrics[[col]] <- 0
        }
      }
      
      cascade_data <- metrics %>%
        select(step, direct_hits, buffer_hits, deenergized) %>%
        pivot_longer(cols = -step, names_to = "impact_type", values_to = "count")
      
      ggplot(cascade_data, aes(x = step, y = count, fill = impact_type)) +
        geom_area(alpha = 0.8) +
        scale_fill_manual(
          values = c("direct_hits" = "#E63946", 
                     "buffer_hits" = "#F77F00", 
                     "deenergized" = "#06AED5"),
          labels = c("Direct Fire Impact", "Buffer Zone Impact", "Cascade Failures")
        ) +
        labs(
          title = "Cascade Impact Analysis",
          x = "Simulation Step",
          y = "Number of Buses",
          fill = "Impact Type"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_minimal()
    })
  })
  
  # Fire Timeline Plot
  output$fire_timeline_plot <- renderPlot({
    req(selected_fire())
    
    tryCatch({
      fire_data <- selected_fire()
      
      if (is.null(fire_data) || nrow(fire_data) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No fire data") +
                 theme_minimal())
      }
      
      fire_timeline <- fire_data %>%
        st_drop_geometry() %>%
        group_by(step) %>%
        summarise(
          total_area = sum(fire_acres, na.rm = TRUE),
          n_polygons = n(),
          max_intensity = first(fire_intensity),
          .groups = 'drop'
        )
      
      if (nrow(fire_timeline) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No timeline data") +
                 theme_minimal())
      }
      
      # Determine if this is a compound event
      fire_names <- unique(fire_data$attr_IncidentName)
      is_compound <- length(fire_names) > 1
      
      title_text <- if (is_compound) {
        paste("Fire Progression: Compound Event (", length(fire_names), " fires)")
      } else {
        paste("Fire Progression:", fire_names[1])
      }
      p1 <- ggplot(fire_timeline, aes(x = step)) +
        geom_col(aes(y = total_area, fill = max_intensity), alpha = 0.8) +
        scale_fill_manual(values = c(
          "Very Low" = "#ffffcc", "Low" = "#fed976", 
          "Moderate" = "#feb24c", "High" = "#fd8d3c", 
          "Extreme" = "#e31a1c", "Unknown" = "#999999"
        )) +
        labs(y = "Total Fire Area (acres)", fill = "Intensity") +
        theme_minimal()
      p2 <- ggplot(fire_timeline, aes(x = step, y = n_polygons)) +
        geom_line(color = "#2b2d42", size = 1.5) +
        geom_point(color = "#2b2d42", size = 3) +
        labs(y = "Number of Polygons", x = "Simulation Step") +
        theme_minimal()
      # Use patchwork if available, otherwise just return the main plot
      if (requireNamespace("patchwork", quietly = TRUE)) {
        (p1 / p2) + patchwork::plot_annotation(title = title_text)
      } else if (requireNamespace("gridExtra", quietly = TRUE)) {
        gridExtra::grid.arrange(p1, p2, ncol = 1, top = title_text)
      } else {
        p1 + labs(title = title_text)
      }
      
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_minimal()
    })
  })
  
  # Vulnerability Plot 
  output$vulnerability_plot <- renderPlot({
    req(values$enhanced_cascade_results)
    
    metrics <- values$enhanced_cascade_results$metrics
    
    if (nrow(metrics) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No vulnerability data") +
               theme_minimal())
    }
    
    # Ensure step column exists
    if (!"step" %in% names(metrics)) {
      metrics <- metrics %>% mutate(step = row_number())
    }
    
    vuln_data <- metrics %>%
      mutate(
        vulnerability_score = cascade_ratio * (1 - vertices_remaining/vcount(graph_original)),
        resilience_loss = 100 - (vertices_remaining/vcount(graph_original) * 100)
      )
    
    vuln_long <- vuln_data %>%
      select(step, cascade_ratio, vulnerability_score, resilience_loss) %>%
      pivot_longer(cols = -step, names_to = "metric", values_to = "value")
    
    ggplot(vuln_long, aes(x = step, y = metric, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "#2E86AB", mid = "#F77F00", high = "#E63946",
        midpoint = 0.5,
        limits = c(0, NA)
      ) +
      labs(
        title = "Grid Vulnerability Analysis",
        x = "Simulation Step",
        y = "Vulnerability Metric",
        fill = "Score"
      ) +
      scale_y_discrete(labels = c(
        "cascade_ratio" = "Cascade Amplification",
        "vulnerability_score" = "Overall Vulnerability",
        "resilience_loss" = "Resilience Loss (%)"
      )) +
      theme_minimal() +
      theme(axis.text.y = element_text(angle = 0, hjust = 1))
  })
  
  # TDA Plot
  output$tda_plot <- renderPlot({
    req(values$tda_results)
    
    tryCatch({
      # First priority: Check if we have ggplot from TDA results
      if (!is.null(values$tda_results$plots_list) && 
          "persistence_after" %in% names(values$tda_results$plots_list)) {
        
        tda_plot <- values$tda_results$plots_list$persistence_after
        
        if (inherits(tda_plot, "ggplot")) {
          return(tda_plot)
        }
      }
      # Fallback: create enhanced plot from persistence data
      if (!is.null(values$tda_results$after_features)) {
        diagram <- values$tda_results$persistence_data
      } else {
        diagram <- values$tda_results$diagram
      }
      
      if (is.null(diagram) || nrow(diagram) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No persistent features found", 
                          size = 6, color = "gray50") +
                 theme_minimal() +
                 labs(title = "TDA Persistence Diagram"))
      }
      
      # Use the new enhanced plotting function
      return(create_enhanced_tda_plot(diagram, "TDA Persistence Diagram"))
      
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("TDA Error:", e$message), 
                 size = 4, color = "red") +
        theme_minimal() +
        labs(title = "TDA Analysis Error")
    })
  })
  
  # ====================================================================
  # OUTPUT FUNCTIONS FOR TDA WORKFLOW
  # ====================================================================
  output$tda_summary_text <- renderText({
    req(values$tda_results)
    
    if (!values$tda_results$success) {
      return(paste("TDA Analysis Failed:", values$tda_results$error))
    }
    
    summary_lines <- c(
      "=== TOPOLOGICAL DATA ANALYSIS SUMMARY ===",
      "",
      paste("Fire Event:", values$tda_results$fire_name),
      paste("Analysis Type:", if(values$tda_results$is_compound_event) "Compound Fire Event" else "Single Fire"),
      "",
      "--- Results ---",
      paste("Features Before Fire:", values$tda_results$before_features),
      paste("Features After Fire:", values$tda_results$after_features),
      paste("Feature Change:", values$tda_results$after_features - values$tda_results$before_features),
      paste("Wasserstein Distance:", round(values$tda_results$wasserstein_distance, 6)),
      "",
      "--- Analysis Parameters ---",
      paste("Analysis Radius:", values$tda_results$analysis_params$analysis_radius_km, "km"),
      paste("Fire Buffer:", values$tda_results$analysis_params$fire_buffer_km, "km"),
      ""
    )
    
    if (!is.null(values$tda_results$plots_list)) {
      summary_lines <- c(summary_lines,
                         "--- Generated Plots ---",
                         paste("Total Plots:", length(values$tda_results$plots_list)),
                         paste("Plot Types:", paste(names(values$tda_results$plots_list), collapse = ", ")),
                         "")
    }
    
    if (values$tda_results$is_compound_event) {
      summary_lines <- c(summary_lines,
                         "--- Compound Event Details ---",
                         paste("Number of Fires:", values$tda_results$fire_count),
                         "Analysis captures combined cascading effects",
                         "Topological signature reflects multi-fire disturbance")
    }
    
    paste(summary_lines, collapse = "\n")
  })
  output$tda_results_available <- reactive({
    !is.null(values$tda_results) && values$tda_results$success
  })
  outputOptions(output, "tda_results_available", suspendWhenHidden = FALSE)
  output$tda_plots_available <- reactive({
    !is.null(values$tda_results) && 
      values$tda_results$success && 
      !is.null(values$tda_results$plots_list) &&
      length(values$tda_results$plots_list) > 0
  })
  outputOptions(output, "tda_plots_available", suspendWhenHidden = FALSE)
  output$cascade_available <- reactive({
    !is.null(values$enhanced_cascade_results) &&
      !is.null(values$enhanced_cascade_results$metrics) &&
      nrow(values$enhanced_cascade_results$metrics) > 0
  })
  outputOptions(output, "cascade_available", suspendWhenHidden = FALSE)
  
  # ====================================================================
  # DAMAGE ASSESSMENT OUTPUTS
  # ====================================================================
  
  # Reactive: compute damage data (reacts to step slider and cascade results)
  damage_data <- reactive({
    req(values$enhanced_cascade_results)
    step <- if (!is.null(input$step)) input$step else NULL
    compute_damage_assessment(values$enhanced_cascade_results, step)
  })
  
  # Bus type damage breakdown summary (colored cards)
  output$damage_type_summary <- renderUI({
    dd <- damage_data()
    if (is.null(dd) || nrow(dd$type_summary) == 0) {
      return(div(class = "alert alert-info", "No damage detected at this step."))
    }
    
    total_gen <- dd$total_gen_lost_mw
    total_load <- dd$total_load_lost_mw
    n_lost <- dd$n_lost
    
    # Color mapping for bus types
    type_colors <- list(
      "Gen + Load" = list(bg = "#f8d7da", border = "#dc3545", icon = "bolt", label = "Critical"),
      "Generator"  = list(bg = "#fff3cd", border = "#ffc107", icon = "industry", label = "High"),
      "Load"       = list(bg = "#d1ecf1", border = "#17a2b8", icon = "plug", label = "Moderate"),
      "Neither"    = list(bg = "#e2e3e5", border = "#6c757d", icon = "circle", label = "Low")
    )
    
    # Build summary cards for each bus type
    type_cards <- lapply(seq_len(nrow(dd$type_summary)), function(i) {
      row <- dd$type_summary[i, ]
      bt <- row$`Bus Type`
      cfg <- type_colors[[bt]] %||% type_colors[["Neither"]]
      
      div(
        style = paste0(
          "background: ", cfg$bg, "; border-left: 4px solid ", cfg$border,
          "; padding: 8px 12px; margin-bottom: 6px; border-radius: 4px;"
        ),
        fluidRow(
          column(5,
                 span(style = "font-weight: bold; font-size: 13px;",
                      icon(cfg$icon), " ", bt),
                 br(),
                 span(style = "font-size: 11px; color: #666;",
                      "Severity: ", cfg$label)
          ),
          column(3,
                 span(style = "font-size: 12px;",
                      strong(row$Count), " bus(es)"),
                 br(),
                 span(style = "font-size: 11px; color: #666;",
                      round(row$`Gen Lost (MW)`, 1), " MW gen")
          ),
          column(4,
                 span(style = "font-size: 12px;",
                      strong(round(row$`Total Impact (MW)`, 1)), " MW total"),
                 br(),
                 span(style = "font-size: 11px; color: #666;",
                      round(row$`Load Lost (MW)`, 1), " MW load")
          )
        )
      )
    })
    
    tagList(
      # Top-level summary
      div(style = "background: #f5f5f5; padding: 10px; border-radius: 6px; margin-bottom: 10px;",
          fluidRow(
            column(4, 
                   h6(style = "margin: 0; color: #333;", icon("times-circle"), " Total Lost"),
                   p(style = "margin: 0; font-size: 18px; font-weight: bold; color: #dc3545;",
                     n_lost, " buses")),
            column(4,
                   h6(style = "margin: 0; color: #333;", icon("industry"), " Gen Lost"),
                   p(style = "margin: 0; font-size: 18px; font-weight: bold; color: #e65100;",
                     round(total_gen, 1), " MW")),
            column(4,
                   h6(style = "margin: 0; color: #333;", icon("plug"), " Load Lost"),
                   p(style = "margin: 0; font-size: 18px; font-weight: bold; color: #1565c0;",
                     round(total_load, 1), " MW"))
          )
      ),
      # Per-type cards
      do.call(tagList, type_cards)
    )
  })
  
  # Affected buses table (sorted by power impact)
  output$damage_bus_table <- DT::renderDataTable({
    dd <- damage_data()
    if (is.null(dd) || nrow(dd$bus_damage) == 0) {
      return(DT::datatable(data.frame(Message = "No buses affected"), 
                           options = list(dom = 't')))
    }
    
    DT::datatable(
      dd$bus_damage,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(5, 'desc')),  # Sort by Total Impact descending
        dom = 'frtip',
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE,
      class = 'compact stripe hover'
    ) %>%
      DT::formatRound(columns = c('Gen (MW)', 'Load (MW)', 'Total Impact (MW)', 'Base kV'), digits = 1) %>%
      DT::formatStyle(
        'Severity',
        backgroundColor = DT::styleEqual(
          c('Critical', 'High', 'Moderate', 'Low'),
          c('#f8d7da', '#fff3cd', '#d1ecf1', '#e2e3e5')
        ),
        fontWeight = 'bold'
      ) %>%
      DT::formatStyle(
        'Type',
        color = DT::styleEqual(
          c('Gen + Load', 'Generator', 'Load', 'Neither'),
          c('#dc3545', '#e65100', '#1565c0', '#6c757d')
        ),
        fontWeight = 'bold'
      )
  })
  
  # Affected branches table
  output$damage_branch_table <- DT::renderDataTable({
    dd <- damage_data()
    if (is.null(dd) || nrow(dd$branch_damage) == 0) {
      return(DT::datatable(data.frame(Message = "No branches affected"),
                           options = list(dom = 't')))
    }
    
    DT::datatable(
      dd$branch_damage,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(5, 'desc')),  # Sort by Rate A descending
        dom = 'frtip',
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE,
      class = 'compact stripe hover'
    ) %>%
      DT::formatRound(columns = c('Rate A (MVA)', 'R (p.u.)', 'X (p.u.)'), digits = 4) %>%
      DT::formatStyle(
        'Status',
        backgroundColor = DT::styleEqual(
          c('Destroyed', 'Degraded'),
          c('#f8d7da', '#fff3cd')
        ),
        fontWeight = 'bold'
      )
  })
  
  # Low-impact fire indicator
  output$low_impact_indicator <- renderUI({
    req(values$enhanced_cascade_results)
    if (isTRUE(values$enhanced_cascade_results$is_low_impact)) {
      div(
        class = "alert alert-warning",
        style = "padding: 10px; margin: 8px 0; font-size: 12px; border-left: 4px solid #ffc107;",
        icon("exclamation-triangle"),
        strong(" Low Impact Fire: "),
        values$enhanced_cascade_results$low_impact_message
      )
    }
  })
  
  # ====================================================================
  # VIEW BUTTON HANDLERS
  # ====================================================================
  
  # View Cascade Progression
  observeEvent(input$view_cascade_progression, {
    tryCatch({
      if (!is.null(values$tda_results) && values$tda_results$success &&
          !is.null(values$tda_results$plots_list)) {
        plot_obj <- values$tda_results$plots_list$cascade_progression %||%
          values$tda_results$plots_list$summary
        if (!is.null(plot_obj) && inherits(plot_obj, "ggplot")) {
          showModal(modalDialog(
            title = "Cascade Progression", size = "l",
            renderPlot({ plot_obj }, height = 550),
            easyClose = TRUE, footer = modalButton("Close")
          ))
        } else {
          showNotification("Cascade progression plot not available.", type = "warning")
        }
      } else {
        showNotification("Run TDA analysis first.", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # View Persistence Diagrams
  observeEvent(input$view_persistence_diagrams, {
    tryCatch({
      if (!is.null(values$tda_results) && values$tda_results$success &&
          !is.null(values$tda_results$plots_list)) {
        plots <- values$tda_results$plots_list
        plot_to_show <- plots$comparison %||% plots$persistence_after %||% plots[[1]]
        
        if (!is.null(plot_to_show) && inherits(plot_to_show, "ggplot")) {
          showModal(modalDialog(
            title = "Persistence Diagrams", size = "l",
            renderPlot({ plot_to_show }, height = 500),
            easyClose = TRUE,
            footer = tagList(
              if (!is.null(plots$persistence_before))
                actionButton("modal_show_before", "Before", class = "btn-info btn-sm"),
              if (!is.null(plots$persistence_after))
                actionButton("modal_show_after", "After", class = "btn-warning btn-sm"),
              if (!is.null(plots$comparison))
                actionButton("modal_show_comparison", "Comparison", class = "btn-success btn-sm"),
              modalButton("Close")
            )
          ))
        } else {
          showNotification("No persistence plots available.", type = "warning")
        }
      } else {
        showNotification("Run TDA analysis first.", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Modal navigation for persistence diagram switching
  observeEvent(input$modal_show_before, {
    plot_obj <- values$tda_results$plots_list$persistence_before
    if (!is.null(plot_obj)) {
      showModal(modalDialog(
        title = "Persistence Diagram - Before Fire", size = "l",
        renderPlot({ plot_obj }, height = 500), easyClose = TRUE,
        footer = tagList(
          actionButton("modal_show_after", "After", class = "btn-warning btn-sm"),
          actionButton("modal_show_comparison", "Comparison", class = "btn-success btn-sm"),
          modalButton("Close"))
      ))
    }
  })
  observeEvent(input$modal_show_after, {
    plot_obj <- values$tda_results$plots_list$persistence_after
    if (!is.null(plot_obj)) {
      showModal(modalDialog(
        title = "Persistence Diagram - After Fire", size = "l",
        renderPlot({ plot_obj }, height = 500), easyClose = TRUE,
        footer = tagList(
          actionButton("modal_show_before", "Before", class = "btn-info btn-sm"),
          actionButton("modal_show_comparison", "Comparison", class = "btn-success btn-sm"),
          modalButton("Close"))
      ))
    }
  })
  observeEvent(input$modal_show_comparison, {
    plot_obj <- values$tda_results$plots_list$comparison
    if (!is.null(plot_obj)) {
      showModal(modalDialog(
        title = "TDA Before/After Comparison", size = "l",
        renderPlot({ plot_obj }, height = 500), easyClose = TRUE,
        footer = tagList(
          actionButton("modal_show_before", "Before", class = "btn-info btn-sm"),
          actionButton("modal_show_after", "After", class = "btn-warning btn-sm"),
          modalButton("Close"))
      ))
    }
  })
  
  # ====================================================================
  # ENHANCED DOWNLOAD HANDLERS
  # ====================================================================
  output$download_tda_plots <- downloadHandler(
    filename = function() {
      fire_name <- if (!is.null(input$fire_events) && length(input$fire_events) > 0) {
        if (length(input$fire_events) > 1) {
          paste("compound", length(input$fire_events), "fires", sep = "_")
        } else {
          gsub("[^A-Za-z0-9]", "_", input$fire_events[1])
        }
      } else if (!is.null(input$fire_event)) {
        gsub("[^A-Za-z0-9]", "_", input$fire_event)
      } else {
        "tda_analysis"
      }
      paste0("tda_plots_", fire_name, "_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      plot_files <- character(0)
      # Save ggplot2 objects from TDA results
      if (!is.null(values$tda_results) && 
          !is.null(values$tda_results$plots_list)) {
        plots_list <- values$tda_results$plots_list
        for (plot_name in names(plots_list)) {
          plot_obj <- plots_list[[plot_name]]
          if (inherits(plot_obj, "ggplot")) {
            plot_file <- file.path(temp_dir, paste0(plot_name, ".png"))
            tryCatch({
              ggsave(plot_file, plot_obj, 
                     width = 12, height = 9, dpi = 300, 
                     bg = "white", device = "png")
              plot_files <- c(plot_files, plot_file)
            }, error = function(e) {
              message("Error saving plot ", plot_name, ": ", e$message)
            })
          }
        }
      }
      # Create summary file
      summary_file <- file.path(temp_dir, "plot_summary.txt")
      fire_info <- if (!is.null(input$fire_events) && length(input$fire_events) > 1) {
        paste("Compound Event:", paste(input$fire_events, collapse = ", "))
      } else if (!is.null(input$fire_events)) {
        input$fire_events[1]
      } else {
        input$fire_event %||% "Unknown"
      }
      
      writeLines(c(
        "=== TDA PLOTS SUMMARY ===",
        paste("Generated:", Sys.time()),
        paste("Total plots:", length(plot_files)),
        paste("Fire analyzed:", fire_info),
        "",
        "Plot files:",
        basename(plot_files)
      ), summary_file)
      
      plot_files <- c(plot_files, summary_file)
      
      if (length(plot_files) > 0) {
        safe_archive(file, plot_files)
      } else {
        # Create empty zip with error message
        error_file <- file.path(temp_dir, "no_plots_available.txt")
        writeLines("No plots available for download", error_file)
        safe_archive(file, c(error_file))
      }
    }
  )
  
  # complete analysis download
  output$download_results <- downloadHandler(
    filename = function() {
      fire_name <- if (!is.null(input$fire_events) && length(input$fire_events) > 0) {
        if (length(input$fire_events) > 1) {
          paste("compound", length(input$fire_events), "fires", sep = "_")
        } else {
          gsub("[^A-Za-z0-9]", "_", input$fire_events[1])
        }
      } else if (!is.null(input$fire_event)) {
        gsub("[^A-Za-z0-9]", "_", input$fire_event)
      } else {
        "analysis"
      }
      paste0("wildfire_grid_analysis_", fire_name, "_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      output_files <- list.files(cfg$outputs_dir, full.names = TRUE)
      
      # Copy existing output files
      if (length(output_files) > 0) {
        file.copy(output_files, temp_dir)
      }
      
      # Save ggplot2 plots
      if (!is.null(values$tda_results) && !is.null(values$tda_results$plots_list)) {
        plots_list <- values$tda_results$plots_list
        
        for (plot_name in names(plots_list)) {
          plot_obj <- plots_list[[plot_name]]
          
          if (inherits(plot_obj, "ggplot")) {
            plot_file <- file.path(temp_dir, paste0(plot_name, ".png"))
            
            tryCatch({
              ggsave(plot_file, plot_obj, 
                     width = 12, height = 9, dpi = 300, 
                     bg = "white", device = "png")
            }, error = function(e) {
              message("Error saving plot ", plot_name, ": ", e$message)
            })
          }
        }
      }
      
      # Generate report
      report_file <- file.path(temp_dir, "enhanced_analysis_report.txt")
      sink(report_file)
      cat("=== WILDFIRE GRID RESILIENCE ANALYSIS REPORT ===\n")
      cat("Generated:", as.character(Sys.time()), "\n")
      cat("Analysis Version: Build 0\n\n")
      # Add TDA summary if available
      if (!is.null(values$tda_results) && values$tda_results$success) {
        cat("--- TDA ANALYSIS RESULTS ---\n")
        cat("Fire Event:", values$tda_results$fire_name, "\n")
        cat("Analysis Type:", if(values$tda_results$is_compound_event) "Compound Fire Event" else "Single Fire", "\n")
        cat("Features Before:", values$tda_results$before_features, "\n")
        cat("Features After:", values$tda_results$after_features, "\n")
        cat("Wasserstein Distance:", values$tda_results$wasserstein_distance, "\n")
        
        if (!is.null(values$tda_results$plots_list)) {
          cat("Generated Plots:", length(values$tda_results$plots_list), "\n")
          cat("Plot Types:", paste(names(values$tda_results$plots_list), collapse = ", "), "\n")
        }
        cat("\n")
      }
      
      # Add cascade summary if available
      if (!is.null(values$enhanced_cascade_results)) {
        cat("--- CASCADE SIMULATION RESULTS ---\n")
        metrics <- values$enhanced_cascade_results$metrics
        if (nrow(metrics) > 0) {
          cat("Total Steps:", nrow(metrics), "\n")
          cat("Total Fire Affected:", sum(metrics$fire_affected), "\n")
          cat("Total Cascade Failures:", sum(metrics$deenergized), "\n")
          cat("Final Grid Size:", tail(metrics$vertices_remaining, 1), "buses\n")
        }
        cat("\n")
      }
      sink()
      # Create zip file
      files_to_zip <- list.files(temp_dir, full.names = TRUE)
      safe_archive(file, files_to_zip)
    }
  )
  
  # Additional download handlers
  output$download_plots_only <- downloadHandler(
    filename = function() {
      paste0("plots_only_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Same as download_tda_plots but different filename
      temp_dir <- tempdir()
      plot_files <- character(0)
      if (!is.null(values$tda_results) && !is.null(values$tda_results$plots_list)) {
        plots_list <- values$tda_results$plots_list
        for (plot_name in names(plots_list)) {
          plot_obj <- plots_list[[plot_name]]
          if (inherits(plot_obj, "ggplot")) {
            plot_file <- file.path(temp_dir, paste0(plot_name, ".png"))
            tryCatch({
              ggsave(plot_file, plot_obj, 
                     width = 12, height = 9, dpi = 300, 
                     bg = "white", device = "png")
              plot_files <- c(plot_files, plot_file)
            }, error = function(e) {
              message("Error saving plot ", plot_name, ": ", e$message)
            })
          }
        }
      }
      
      if (length(plot_files) > 0) {
        safe_archive(file, plot_files)
      } else {
        error_file <- file.path(temp_dir, "no_plots_available.txt")
        writeLines("No plots available for download", error_file)
        safe_archive(file, c(error_file))
      }
    }
  )
  
  output$download_gis <- downloadHandler(
    filename = function() {
      paste0("gis_data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create GIS data export (placeholder)
      temp_dir <- tempdir()
      gis_file <- file.path(temp_dir, "gis_data_info.txt")
      writeLines(c(
        "GIS Data Export",
        "Generated:", as.character(Sys.time()),
        "",
        "Note: GIS data export functionality would include:",
        "- Fire perimeter shapefiles",
        "- Bus location shapefiles", 
        "- Grid network shapefiles",
        "- Analysis area boundaries"
      ), gis_file)
      
      safe_archive(file, c(gis_file))
    }
  )
  
  output$download_matrices <- downloadHandler(
    filename = function() {
      paste0("tda_matrices_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create matrices export (placeholder)
      temp_dir <- tempdir()
      matrices_file <- file.path(temp_dir, "matrices_info.txt")
      writeLines(c(
        "TDA Matrices Export",
        "Generated:", as.character(Sys.time()),
        "",
        "Note: Matrix export would include:",
        "- Distance matrices",
        "- Persistence diagrams (CSV)",
        "- Adjacency matrices",
        "- Power difference matrices"
      ), matrices_file)
      
      safe_archive(file, c(matrices_file))
    }
  )
}