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
# - Download handlers (results, plots, GIS data, matrices)
#
# Brandon Calvario
# =================================================================================================

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
        zip(file, plot_files, flags = "-j")
      } else {
        # Create empty zip with error message
        error_file <- file.path(temp_dir, "no_plots_available.txt")
        writeLines("No plots available for download", error_file)
        zip(file, error_file, flags = "-j")
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
      zip(file, files_to_zip, flags = "-j")
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
        zip(file, plot_files, flags = "-j")
      } else {
        error_file <- file.path(temp_dir, "no_plots_available.txt")
        writeLines("No plots available for download", error_file)
        zip(file, error_file, flags = "-j")
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
      
      zip(file, gis_file, flags = "-j")
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
      
      zip(file, matrices_file, flags = "-j")
    }
  )
}