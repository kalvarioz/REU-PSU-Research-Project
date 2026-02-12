# =================================================================================================
# modules/event_handlers.R
# 
# Event Handler Functions (Initialization Safe)
#
# This module contains functions for:
# - State selection handling
# - Intensity selection handling
# - Filter change handling
# - Multiple fire selection handling
# - Cascade execution handling
# - TDA analysis handling
#
# Brandon Calvario
# =================================================================================================

# ====================================================================
# EVENT HANDLERS
# ====================================================================


handle_state_selection <- function(input, session, selected_state) {
  observeEvent(input$state_select, {
    if (!is.null(input$state_select) && input$state_select != "") {
      setup_parallel_processing()
      selected_state(input$state_select)
      updateSelectInput(session, "fire_intensity_select", selected = "")
      updateCheckboxGroupInput(session, "fire_events", selected = character(0))
      
      showNotification(
        paste("Selected state:", tools::toTitleCase(input$state_select), ". Parallel environment reset."), 
        type = "message", duration = 4
      )
    }
  }, ignoreInit = TRUE)
}

handle_intensity_selection <- function(input, session) {
  observeEvent(input$fire_intensity_select, {
    if (!is.null(input$fire_intensity_select)) {
      updateSelectInput(session, "fire_event", selected = "")
    }
  })
}

handle_multiple_fire_selection <- function(input, session, get_available_fire_events) {
  # Select all fires button
  observeEvent(input$select_all_fires, {
    req(input$state_select, input$fire_intensity_select)
    
    available_fires <- get_available_fire_events()
    if (nrow(available_fires) > 0) {
      all_fire_names <- available_fires$attr_IncidentName
      updateCheckboxGroupInput(session, "fire_events", selected = all_fire_names)
      
      showNotification(
        paste("Selected all", length(all_fire_names), "fires for compound analysis"), 
        type = "message", duration = 5
      )
    }
  })
  
  # Clear all fires button
  observeEvent(input$clear_fire_selection, {
    updateCheckboxGroupInput(session, "fire_events", selected = character(0))
    showNotification("Fire selection cleared", type = "message", duration = 3)
  })
}

handle_filter_changes <- function(input, session) {
  # Existing filter change logic
  observeEvent(c(input$fire_intensity_select, input$fuel_type_filter, 
                 input$fuel_category_filter, input$landowner_category_filter, 
                 input$landowner_type_filter), {
                   if (!is.null(input$fire_intensity_select) && input$fire_intensity_select != "") {
                     updateSelectInput(session, "fire_event", selected = "")
                     # Clear multiple selection
                     updateCheckboxGroupInput(session, "fire_events", selected = character(0))
                   }
                 }, ignoreInit = TRUE)
  
  # Existing clear filters logic
  observeEvent(input$clear_all_filters, {
    updateSelectInput(session, "fuel_type_filter", selected = "")
    updateSelectInput(session, "fuel_category_filter", selected = "")
    updateSelectInput(session, "landowner_category_filter", selected = "")
    updateSelectInput(session, "landowner_type_filter", selected = "")
    
    showNotification("All filters cleared", type = "message", duration = 3)
  })
  
  # Auto-clear fire selection when state/intensity changes
  observeEvent(c(input$state_select, input$fire_intensity_select), {
    updateCheckboxGroupInput(session, "fire_events", selected = character(0))
    # Also clear legacy single selection
    updateSelectInput(session, "fire_event", selected = "")
  }, ignoreInit = TRUE)
}

handle_fire_selection <- function(input, selected_fire) {
  # Fire selection is handled through reactive expressions
}

handle_cascade_execution <- function(input, values, selected_fire, session) {
  observeEvent(input$run_cascade, {
    # Check connection status before running
    connection_count <- monitor_connections()
    if (connection_count > 10) {
      message("High connection count detected, cleaning up...")
      cleanup_parallel_resources(force_cleanup = TRUE)
      Sys.sleep(0.5)
    }
    
    shinyjs::runjs("
      var btn = document.getElementById('run_cascade');
      if (btn) btn.setAttribute('data-clicked', 'true');
    ")
    req(selected_fire(), input$buffer_km, values$system_ready)
    
    withProgress(message = 'Running SAFE cascade simulation...', {
      
      tryCatch({
        fire_data <- selected_fire()
        fire_name <- unique(fire_data$attr_IncidentName)[1]
        fire_validation <- validate_fire_data(fire_data)
        if (!fire_validation$valid) {
          showNotification(
            div(
              h5("Fire Data Error"),
              p(fire_validation$error),
              div(style = "text-align: center; margin-top: 10px;",
                  actionButton("dismiss_fire_error", "Try Different Fire", class = "btn-warning btn-sm")
              )
            ), 
            type = "error", duration = 15, closeButton = TRUE
          )
          return()
        }
        
        bus_validation <- validate_bus_data(buses_sf)
        if (!bus_validation$valid) {
          showNotification(
            div(
              h5("Bus Data Error"),  
              p(bus_validation$error),
              p("This is a system configuration issue. Please check your data loading."),
              div(style = "text-align: center; margin-top: 10px;",
                  actionButton("dismiss_bus_error", "OK", class = "btn-danger btn-sm")
              )
            ), 
            type = "error", duration = 15, closeButton = TRUE
          )
          return()
        }
        
        message("Starting SAFE cascade simulation:")
        message("Fire: ", fire_name)
        message("Fire polygons: ", nrow(fire_data))
        message("Buffer: ", input$buffer_km, " km")
        message("Max steps: ", cfg$simulation_steps)
        # Update progress
        incProgress(0.1, detail = "Setting up safe parallel processing...")
        setup_parallel_processing(max_workers = 4)
        incProgress(0.2, detail = "Running cascade simulation...")
        result <- tryCatch({
          run_enhanced_fire_cascade(
            graph = graph_original,
            buses_sf = buses_sf,
            fire_data = fire_data,
            buffer_km = input$buffer_km,
            steps = cfg$simulation_steps,
            use_parallel = TRUE,
            parallel_method = "multisession"
          )
        }, error = function(e) {
          message("Parallel cascade error: ", e$message)
          # Try cleanup and retry once
          cleanup_parallel_resources(force_cleanup = TRUE)
          Sys.sleep(1)
          message("Retrying with sequential processing...")
          run_enhanced_fire_cascade(
            graph = graph_original,
            buses_sf = buses_sf,
            fire_data = fire_data,
            buffer_km = input$buffer_km,
            steps = cfg$simulation_steps,
            use_parallel = FALSE
          )
        })
        # Check if result indicates failure
        if (!is.null(result$success) && !result$success) {
          stop(result$error)
        }
        incProgress(0.6, detail = "Processing results...")
        # Tag results for TDA compatibility
        result$fire_name <- fire_name
        result$buffer_km <- input$buffer_km
        result$timestamp <- Sys.time()
        result$parallel_used <- TRUE
        values$cascade_results <- result
        values$enhanced_cascade_results <- result
        incProgress(0.7, detail = "Updating interface...")
        # Update slider
        max_steps <- min(length(result$graphs) - 1, cfg$simulation_steps)
        updateSliderInput(session, "step", min = 1, max = max_steps, value = 1)
        
        # --- Compute and draw fire impact buffer on map ---
        tryCatch({
          buffer_km <- input$buffer_km
          fire_geom <- fire_data
          
          buffer_dist <- if (st_is_longlat(fire_geom)) buffer_km * 1000 else buffer_km / 111
          fire_buffer_geom <- st_buffer(st_union(fire_geom), dist = buffer_dist)
          values$fire_impact_buffer <- fire_buffer_geom
          values$fire_impact_buffer_km <- buffer_km
          
          # Also buffer fire centers if available
          if ("has_center_point" %in% names(fire_data)) {
            fires_with_centers <- fire_data %>%
              filter(has_center_point == TRUE, !is.na(attr_InitialLatitude), !is.na(attr_InitialLongitude))
            if (nrow(fires_with_centers) > 0) {
              fire_centers_sf <- st_as_sf(fires_with_centers,
                                          coords = c("attr_InitialLongitude", "attr_InitialLatitude"), crs = st_crs(fire_geom))
              values$fire_center_buffers <- st_buffer(fire_centers_sf, dist = buffer_dist)
            }
          }
          
          # Draw buffer on map
          proxy <- leafletProxy("map") %>% clearGroup("impact_zones")
          proxy <- proxy %>%
            addPolygons(
              data = st_as_sf(data.frame(geometry = fire_buffer_geom)),
              group = "impact_zones",
              color = "#FF8C00", weight = 2, dashArray = "8,6",
              fillColor = "#FF8C00", fillOpacity = 0.08,
              popup = paste0("Fire Impact Buffer: ", buffer_km, " km")
            )
          if (!is.null(values$fire_center_buffers)) {
            proxy <- proxy %>%
              addPolygons(
                data = values$fire_center_buffers,
                group = "impact_zones",
                color = "#DC3545", weight = 2, dashArray = "5,5",
                fillColor = "#DC3545", fillOpacity = 0.05,
                popup = ~paste0("Fire Center Buffer: ", buffer_km, " km<br>Fire: ", attr_IncidentName)
              )
          }
          message("Impact buffer drawn on map (", buffer_km, " km)")
        }, error = function(e) {
          message("Warning: Could not draw impact buffer: ", e$message)
        })
        
        incProgress(0.9, detail = "Computing damage assessment...")
        
        # Compute damage summary stats
        total_affected <- sum(sapply(result$buses_lost_per_step, length))
        final_buses <- vcount(result$graphs[[length(result$graphs)]])
        is_low_impact <- total_affected <= 2
        
        # Compute damage breakdown by bus type
        damage_summary <- tryCatch({
          all_lost <- unique(unlist(result$buses_lost_per_step))
          if (length(all_lost) > 0 && exists("bus_info")) {
            lost_info <- bus_info %>% st_drop_geometry() %>% filter(bus_i %in% all_lost)
            list(
              gen_load_lost = sum(lost_info$bus_type == "Gen + Load"),
              gen_lost = sum(lost_info$bus_type == "Generator"),
              load_lost = sum(lost_info$bus_type == "Load"),
              total_gen_mw = round(sum(lost_info$total_gen, na.rm = TRUE), 1),
              total_load_mw = round(sum(lost_info$load_mw, na.rm = TRUE), 1)
            )
          } else {
            list(gen_load_lost = 0, gen_lost = 0, load_lost = 0, total_gen_mw = 0, total_load_mw = 0)
          }
        }, error = function(e) {
          list(gen_load_lost = 0, gen_lost = 0, load_lost = 0, total_gen_mw = 0, total_load_mw = 0)
        })
        
        incProgress(1.0, detail = "Complete!")
        
        # Clean up connections
        connection_count_after <- monitor_connections()
        if (connection_count_after > connection_count + 5) {
          message("Connection count increased significantly, cleaning up...")
          cleanup_parallel_resources()
        }
        
        # Store low-impact flag for UI
        values$enhanced_cascade_results$is_low_impact <- is_low_impact
        if (is_low_impact) {
          values$enhanced_cascade_results$low_impact_message <- paste0(
            "Low impact: only ", total_affected, " bus(es) affected. ",
            "Fire may be too far from grid infrastructure.")
        }
        
        if (is_low_impact) {
          # LOW IMPACT NOTIFICATION
          showNotification(
            div(
              h5(style = "color: #856404;", icon("info-circle"), " Low Impact Fire"),
              div(style = "background: #fff3cd; padding: 10px; border-radius: 6px; border-left: 4px solid #ffc107; margin: 8px 0;",
                  p(style = "margin: 0; font-weight: bold; color: #856404;",
                    "This fire is unlikely to cause major grid damage."),
                  p(style = "margin: 4px 0 0 0; font-size: 12px; color: #856404;",
                    "Only ", strong(total_affected), " bus(es) affected within the ",
                    strong(input$buffer_km), " km impact buffer.")
              ),
              p("Fire: ", strong(fire_name)),
              p("Final Grid: ", strong(final_buses), " / ", strong(vcount(graph_original)), " buses intact"),
              if (total_affected == 0) {
                div(style = "background: #d4edda; padding: 8px; border-radius: 4px; margin-top: 8px;",
                    p(style = "margin: 0; font-size: 12px; color: #155724;",
                      icon("check-circle"), " No buses in fire impact range. ",
                      "Consider increasing the impact buffer."))
              },
              br(),
              div(style = "text-align: center;",
                  actionButton("run_tda_after_cascade", "Run TDA Analysis Anyway",
                               class = "btn-warning btn-sm", 
                               onclick = "document.getElementById('run_wildfire_tda').click();"))
            ),
            type = "warning", duration = 20, closeButton = TRUE
          )
        } else {
          # NORMAL IMPACT NOTIFICATION with damage breakdown
          showNotification(
            div(
              h5(icon("check-circle"), " Cascade Complete!"),
              p("Fire: ", strong(fire_name)),
              p("Buffer: ", strong(input$buffer_km), " km"),
              div(style = "background: #f8f9fa; padding: 8px; border-radius: 4px; margin: 5px 0;",
                  p(style = "margin: 2px 0; font-size: 12px;",
                    icon("bolt"), " Buses Lost: ", strong(total_affected),
                    " (", final_buses, " remaining)"),
                  if (damage_summary$gen_load_lost > 0) {
                    p(style = "margin: 2px 0; font-size: 12px; color: #dc3545;",
                      "  Gen+Load (Critical): ", strong(damage_summary$gen_load_lost))
                  },
                  if (damage_summary$gen_lost > 0) {
                    p(style = "margin: 2px 0; font-size: 12px; color: #e65100;",
                      "  Generator (High): ", strong(damage_summary$gen_lost))
                  },
                  if (damage_summary$load_lost > 0) {
                    p(style = "margin: 2px 0; font-size: 12px; color: #1565c0;",
                      "  Load (Moderate): ", strong(damage_summary$load_lost))
                  },
                  p(style = "margin: 4px 0 0 0; font-size: 11px; color: #666;",
                    "Power lost: ", damage_summary$total_gen_mw, " MW gen + ",
                    damage_summary$total_load_mw, " MW load")
              ),
              br(),
              div(style = "text-align: center;",
                  actionButton("run_tda_after_cascade", "Run TDA Analysis",
                               class = "btn-success", 
                               onclick = "document.getElementById('run_wildfire_tda').click();"))
            ),
            type = "message", duration = 15, closeButton = TRUE
          )
        }
        
      }, error = function(e) {
        # Clean up on error
        cleanup_parallel_resources(force_cleanup = TRUE)
        
        showNotification(
          div(
            h5("Cascade Simulation Failed"),
            p("Fire: ", if(exists("fire_name")) fire_name else "Unknown"),
            p("Error: ", e$message),
            br(),
            p("This may be due to connection issues when switching states quickly."),
            p("Try waiting a moment and running the analysis again."),
            br(),
            div(style = "text-align: center;",
                actionButton("retry_cascade", "Retry Analysis", 
                             class = "btn-warning")
            )
          ),
          type = "error", 
          duration = 20,
          closeButton = TRUE
        )
        message("Cascade error details: ", e$message)
      })
    })
  })
}


render_parameter_explanation_ui <- function(input, output) {
  output$parameter_explanation_dynamic <- renderUI({
    div(class = "summary-box", style = "background: #f8f9fa; border-color: #dee2e6;",
        h6(icon("question-circle"), " Parameter Guide"),
        
        div(style = "margin-bottom: 10px;",
            strong("Impact Buffer (", if (!is.null(input$buffer_km)) paste0(input$buffer_km, " km") else "2 km", "):"),
            br(),
            "Determines which buses are directly affected by fire during cascade simulation.",
            br(),
            span("Smaller values = more localized fire impact", style = "font-size: 11px; color: #329ea8;")
        ),
        
        div(style = "margin-bottom: 10px;",
            strong("Analysis Radius (", if (!is.null(input$proximity_km)) paste0(input$proximity_km, " km") else "30 km", "):"),
            br(),
            "Defines the area around fire for topological analysis (TDA scope).",
            br(),
            span("Larger values = broader topology analysis", style = "font-size: 11px; color: #281e87;")
        ),
        
        if (!is.null(input$buffer_km) && !is.null(input$proximity_km)) {
          if (input$proximity_km < input$buffer_km) {
            div(class = "alert alert-warning", style = "padding: 5px; margin-top: 10px; font-size: 11px;",
                icon("exclamation-triangle"), 
                " Analysis radius is smaller than impact buffer. Consider increasing analysis radius.")
          } else {
            div(style = "color: #28a745; font-size: 11px; margin-top: 10px;",
                icon("check-circle"), " Parameters are well-configured.")
          }
        }
    )
  })
}

handle_tda_analysis<- function(input, values, selected_fire, session) {
  observeEvent(input$run_wildfire_tda, {
    req(selected_fire(), values$system_ready)
    
    # *** NEW SAFETY CHECK ***
    # Ensure the healthy state matrix is loaded before proceeding.
    if (!exists("healthy_state_matrix") || is.null(healthy_state_matrix)) {
      showNotification("Critical Error: The baseline 'healthy state' power matrix is not loaded. Please restart the application.", type = "error", duration = 20)
      return()
    }
    
    fire_data <- selected_fire()
    fire_names <- unique(fire_data$attr_IncidentName)
    is_compound_event <- length(fire_names) > 1
    
    message("=== STARTING ENHANCED TDA ANALYSIS ===")
    message("Fire names: ", paste(fire_names, collapse = ", "))
    message("Is compound: ", is_compound_event)
    message("Fire polygons: ", nrow(fire_data))
    
    # Check fire data quality
    if (nrow(fire_data) == 0) {
      showNotification("No fire data available for analysis", type = "error")
      return()
    }
    
    # Validate fire geometry
    if (any(!st_is_valid(fire_data))) {
      message("Fixing invalid fire geometries...")
      fire_data <- st_make_valid(fire_data)
    }
    
    if (is_compound_event) {
      fire_display_text <- paste("Compound Event:", length(fire_names), "fires")
      fire_detail_text <- paste("Fires:", paste(fire_names, collapse = ", "))
      analysis_type <- "COMPOUND FIRE EVENT"
    } else {
      fire_display_text <- fire_names[1]
      fire_detail_text <- paste("Single fire:", fire_names[1])
      analysis_type <- "SINGLE FIRE EVENT"
    }
    
    # Get parameters with validation
    analysis_radius <- if (!is.null(input$proximity_km) && input$proximity_km > 0) {
      input$proximity_km
    } else {
      30  # Default
    }
    
    fire_buffer <- if (!is.null(input$buffer_km) && input$buffer_km > 0) {
      input$buffer_km
    } else {
      2  # Default
    }
    
    message("Analysis parameters:")
    message("Analysis radius: ", analysis_radius, " km")
    message("Fire buffer: ", fire_buffer, " km")
    
    # Check for compatible cascade results
    cascade_available <- !is.null(values$enhanced_cascade_results)
    cascade_compatible <- FALSE
    
    if (cascade_available) {
      existing_buffer <- values$enhanced_cascade_results$buffer_km %||% NA
      existing_fire_names <- values$enhanced_cascade_results$fire_name %||% NA
      
      cascade_compatible <- (!is.na(existing_buffer) && existing_buffer == fire_buffer) &&
        (!is.na(existing_fire_names))
      
      message("Cascade compatibility check:")
      message("Available: ", cascade_available)
      message("Compatible: ", cascade_compatible)
    }
    
    showModal(modalDialog(
      title = paste("Enhanced TDA Analysis -", analysis_type),
      div(
        div(style = "background: #e3f2fd; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
            h5(style = "margin: 0 0 8px 0; color: #1565c0;", 
               icon(if(is_compound_event) "fire-alt" else "fire"), 
               " ", fire_display_text),
            p(style = "margin: 0; font-size: 13px; color: #424242;", fire_detail_text),
            if (is_compound_event) {
              div(style = "margin-top: 8px; padding: 6px; background: #fff3e0; border-radius: 4px;",
                  p(style = "margin: 0; font-size: 12px; color: #e65100; font-weight: bold;",
                    "ENHANCED COMPOUND EVENT ANALYSIS: Multi-fire cascading topology"))
            }
        ),
        
        div(style = "background: #f5f5f5; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
            h6(style = "margin: 0 0 8px 0;", "Enhanced Analysis Parameters:"),
            fluidRow(
              column(6, p(style = "margin: 0; font-size: 12px;", 
                          strong("Analysis Radius: "), analysis_radius, " km")),
              column(6, p(style = "margin: 0; font-size: 12px;", 
                          strong("Fire Buffer: "), fire_buffer, " km"))
            ),
            if (cascade_compatible) {
              p(style = "margin: 8px 0 0 0; font-size: 11px; color: #2e7d32;",
                "-- Using existing compatible cascade results")
            } else {
              p(style = "margin: 8px 0 0 0; font-size: 11px; color: #d84315;",
                "-- Will run new enhanced cascade simulation")
            }
        ),
        
        div(class = "progress", style = "margin-bottom: 15px;",
            div(class = "progress-bar progress-bar-striped progress-bar-animated", 
                role = "progressbar", style = "width: 100%", 
                "Running enhanced TDA analysis with debugging...")),
        
        div(style = "font-size: 12px; color: #000000;",
            if (is_compound_event) {
              div(
                p("Phase 1: Combined multi-fire impact analysis"),
                p("Phase 2: Enhanced cascade simulation with debugging"),
                p("Phase 3: Before/after topology comparison"),
                p("Phase 4: Enhanced visualization with ggplot2"),
                p("Phase 5: Detailed Wasserstein distance calculation")
              )
            } else {
              div(
                p("Phase 1: Fire impact analysis with validation"),
                p("Phase 2: Enhanced cascade simulation"),
                p("Phase 3: TDA topology comparison with debugging"),
                p("Phase 4: Advanced visualization generation"),
                p("Phase 5: Wasserstein distance with logging")
              )
            },
            br(),
            p(strong("Enhanced analysis includes detailed debugging and validation."), 
              style = "text-align: center; color: #1976d2;")
        )
      ),
      footer = tagList(modalButton("Cancel Analysis")),
      easyClose = FALSE,
      size = "l"
    ))
    
    # Run enhanced analysis
    tryCatch({
      message("Calling run_fixed_smart_tda_workflow...")
      
      analysis_results <- run_fixed_smart_tda_workflow(
        fire_data = fire_data,
        bus_info = buses_sf,
        graph_original = graph_original,
        analysis_radius_km = analysis_radius,
        fire_impact_buffer_km = fire_buffer,
        simulation_steps = cfg$simulation_steps,
        use_existing_cascade = cascade_compatible,
        existing_cascade_results = if (cascade_compatible) values$enhanced_cascade_results else NULL,
        generate_plots = TRUE
      )
      
      removeModal()
      
      if (analysis_results$success) {
        values$tda_results <- analysis_results
        
        message("=== TDA ANALYSIS COMPLETED SUCCESSFULLY ===")
        message("Wasserstein distance: ", analysis_results$wasserstein_distance)
        message("Before features: ", analysis_results$before_features)
        message("After features: ", analysis_results$after_features)
        
        # --- Draw analysis radius circle on the map ---
        tryCatch({
          # Compute centroid of fire geometry for radius center
          fire_centroid <- st_centroid(st_union(fire_data))
          centroid_coords <- st_coordinates(fire_centroid)
          
          # Buffer for analysis radius (larger circle, different from impact buffer)
          radius_dist <- if (st_is_longlat(fire_data)) {
            analysis_radius * 1000  # meters for longlat
          } else {
            analysis_radius / 111  # approximate degrees
          }
          analysis_circle <- st_buffer(fire_centroid, dist = radius_dist)
          
          # Store for persistence during map updates
          values$analysis_radius_circle <- analysis_circle
          values$analysis_radius_km <- analysis_radius
          values$analysis_radius_center <- centroid_coords
          
          # Draw on map
          leafletProxy("map") %>%
            clearGroup("analysis_radius") %>%
            addPolygons(
              data = st_as_sf(data.frame(geometry = analysis_circle)),
              group = "analysis_radius",
              color = "#6A0DAD",       # Purple
              weight = 2.5,
              dashArray = "10,8",      # Long dash
              fillColor = "#9B59B6",
              fillOpacity = 0.04,
              popup = paste0(
                "<b>TDA Analysis Radius</b><br>",
                "Radius: ", analysis_radius, " km<br>",
                "Buses in this area are used for<br>",
                "before/after topology comparison"
              )
            ) %>%
            # Add center point marker
            addCircleMarkers(
              lng = centroid_coords[1], lat = centroid_coords[2],
              group = "analysis_radius",
              radius = 4,
              color = "#6A0DAD",
              fillColor = "#9B59B6",
              fillOpacity = 0.8,
              stroke = TRUE, weight = 2,
              popup = paste0(
                "<b>Analysis Center</b><br>",
                "Lat: ", round(centroid_coords[2], 4), "<br>",
                "Lon: ", round(centroid_coords[1], 4), "<br>",
                "Analysis Radius: ", analysis_radius, " km"
              )
            )
          
          message("Analysis radius drawn on map (", analysis_radius, " km)")
        }, error = function(e) {
          message("Warning: Could not draw analysis radius: ", e$message)
        })
        
        # Enhanced success notification
        showNotification(
          div(
            h5(paste("Enhanced TDA Analysis Complete!", 
                     if(is_compound_event) "(COMPOUND EVENT)" else "")),
            
            div(style = "background: #e8f5e8; padding: 8px; border-radius: 4px; margin: 8px 0;",
                if (is_compound_event) {
                  div(
                    p(style = "margin: 0; font-weight: bold;", 
                      "    Compound Fire Event: ", length(fire_names), " fires analyzed"),
                    p(style = "margin: 2px 0 0 0; font-size: 11px;", 
                      "Fires: ", paste(fire_names, collapse = ", "))
                  )
                } else {
                  p(style = "margin: 0; font-weight: bold;", 
                    "Fire: ", strong(fire_names[1]))
                }
            ),
            
            fluidRow(
              column(6,
                     p(style = "margin: 2px 0; font-size: 12px;",
                       "    Features Before: ", analysis_results$before_features),
                     p(style = "margin: 2px 0; font-size: 12px;",
                       "    Features After: ", analysis_results$after_features)
              ),
              column(6,
                     p(style = "margin: 2px 0; font-size: 12px;",
                       "    Wasserstein Distance: ", round(analysis_results$wasserstein_distance, 4)),
                     p(style = "margin: 2px 0; font-size: 12px;",
                       "    Analysis Type: ", if(is_compound_event) "Compound" else "Single")
              )
            ),
            
            br(),
            div(style = "text-align: center;",
                p("Enhanced plots available in RStudio Plots panel!", 
                  style = "color: #28a745; font-weight: bold;"),
                p("Detailed debugging information logged to console", 
                  style = "color: #17a2b8; font-size: 11px;")
            ),
            
            if (is_compound_event) {
              div(style = "margin-top: 8px; padding: 6px; background: #fff3e0; border-radius: 4px;",
                  p(style = "margin: 0; font-size: 11px; color: #e65100;",
                    "Enhanced compound fire analysis captures multi-fire cascading topology patterns"))
            }
          ),
          type = "message", 
          duration = 30,  # Longer for enhanced analysis
          closeButton = TRUE
        )
        
      } else {
        values$tda_results <- list(
          success = FALSE, 
          error = analysis_results$error,
          fire_name = fire_display_text,
          is_compound_event = is_compound_event
        )
        
        showNotification(
          div(
            h5("(!!!) TDA Analysis Failed"),
            p("Fire(s): ", strong(fire_display_text)),
            if (is_compound_event) {
              p("Event Type: ", strong("Compound Event (", length(fire_names), " fires)"))
            },
            p("Error: ", analysis_results$error),
            p("Check console for detailed debugging information.", style = "color: #17a2b8;")
          ),
          type = "error", 
          duration = 20,
          closeButton = TRUE
        )
      }
      
    }, error = function(e) {
      removeModal()
      
      message("(!!!!) TDA ANALYSIS ERROR")
      message("Error details: ", e$message)
      message("Fire: ", fire_display_text)
      message("Compound: ", is_compound_event)
      
      values$tda_results <- list(
        success = FALSE, 
        error = paste("Enhanced analysis error:", e$message),
        fire_name = fire_display_text,
        is_compound_event = is_compound_event
      )
      
      showNotification(
        div(
          h5("Critical Enhanced Analysis Error"),
          p("Fire(s): ", strong(fire_display_text)),
          if (is_compound_event) {
            p("Event Type: ", strong("Compound Event"))
          },
          p("Error: ", e$message),
          p("See console for detailed debugging trace", style = "color: #dc3545; font-weight: bold;")
        ),
        type = "error", 
        duration = 25,
        closeButton = TRUE
      )
      
      message("Full error traceback:")
      message(capture.output(traceback()))
    })
  })
}
message(" --Enhanced server functions loaded with TDA debugging")