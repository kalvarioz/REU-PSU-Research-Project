# =================================================================================================
# modules/ui_renderers.R
# 
# Dynamic UI Rendering Functions
#
# This module contains functions for:
# - Rendering state selection UI
# - Rendering intensity selection UI
# - Rendering filter options UI
# - Rendering fire selection UI
# - Rendering analysis options UI
# - Rendering download options UI
#
# Brandon Calvario
# =================================================================================================

# ====================================================================
# DYNAMIC UI RENDERING
# ====================================================================

render_state_selection_ui <- function(input, output) {
  output$state_summary_dynamic <- renderUI({
    if (is.null(input$state_select) || input$state_select == "") {
      return(div(style = "color: #000000; font-size: 12px; margin-top: 10px;",
                 "Select a western state to begin analysis"))
    }
    
    state_fires <- wfigs_perimeters %>%
      filter(tolower(attr_POOState) == tolower(input$state_select))
    
    if (nrow(state_fires) == 0) {
      return(div(class = "summary-box", style = "background: #f8d7da; border-color: #f5c6cb;",
                 icon("exclamation-triangle"), " No fires found in ", tools::toTitleCase(input$state_select)))
    }
    
    unique_fires <- length(unique(state_fires$attr_IncidentName))
    total_area <- sum(state_fires$fire_acres, na.rm = TRUE)
    intensity_breakdown <- table(state_fires$fire_intensity)
    
    div(class = "summary-box",
        h6(icon("map-marker-alt"), " ", strong(tools::toTitleCase(input$state_select)), " Overview"),
        tags$ul(style = "margin: 5px 0; padding-left: 20px;",
                tags$li(strong(unique_fires), " fire events"),
                tags$li(strong(format(round(total_area), big.mark = ",")), " total acres burned"),
                tags$li("Most common intensity: ", strong(names(sort(intensity_breakdown, decreasing = TRUE))[1]))
        ),
        div(style = "margin-top: 8px; font-size: 12px; color: #000000;",
            "Proceed to Step 2 to select fire intensity level.")
    )
  })
}

render_intensity_selection_ui <- function(input, output) {
  output$intensity_dropdown_dynamic <- renderUI({
    req(input$state_select)
    
    fires_in_state <- wfigs_perimeters %>%
      filter(tolower(attr_POOState) == tolower(input$state_select))
    
    if (nrow(fires_in_state) == 0) {
      return(div(class = "alert alert-warning", "No fires available in selected state."))
    }
    
    intensity_counts <- fires_in_state %>%
      group_by(fire_intensity) %>%
      summarise(
        fire_count = length(unique(attr_IncidentName)),
        record_count = n(),
        total_area = sum(fire_acres, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(match(fire_intensity, c("Extreme", "High", "Moderate", "Low", "Very Low", "Unknown")))
    
    intensity_choices <- setNames(
      intensity_counts$fire_intensity,
      paste0(intensity_counts$fire_intensity, " (", 
             intensity_counts$fire_count, " fires, ",
             format(round(intensity_counts$total_area), big.mark = ","), " acres)")
    )
    
    selectInput("fire_intensity_select", 
                label = NULL,
                choices = c("Choose fire intensity level..." = "", intensity_choices),
                selected = "")
  })
  
  output$intensity_summary_dynamic <- renderUI({
    req(input$fire_intensity_select)
    
    if (input$fire_intensity_select == "") return(NULL)
    
    filtered_fires <- wfigs_perimeters %>%
      filter(tolower(attr_POOState) == tolower(input$state_select),
             fire_intensity == input$fire_intensity_select)
    
    if (nrow(filtered_fires) == 0) {
      div(class = "summary-box", style = "background: #f8d7da; border-color: #f5c6cb;",
          icon("exclamation-triangle"), " No fires found with selected criteria.")
    } else {
      unique_fires <- length(unique(filtered_fires$attr_IncidentName))
      avg_area <- mean(filtered_fires$fire_acres, na.rm = TRUE)
      total_area <- sum(filtered_fires$fire_acres, na.rm = TRUE)
      
      div(class = "summary-box",
          h6(icon("fire"), " ", strong(input$fire_intensity_select), " Intensity Fires"),
          tags$ul(style = "margin: 5px 0; padding-left: 20px;",
                  tags$li(strong(unique_fires), " fire events available"),
                  tags$li("Average fire size: ", strong(format(round(avg_area), big.mark = ",")), " acres"),
                  tags$li("Total area: ", strong(format(round(total_area), big.mark = ",")), " acres")
          ),
          div(style = "margin-top: 8px; font-size: 12px; color: #666;",
              "Proceed to Step 3 to apply additional filters, or Step 4 to select a specific fire.")
      )
    }
  })
}

render_filter_options_ui <- function(input, output, values, get_filtered_fire_data_for_filters) {
  # Fuel Type Filter
  output$fuel_type_filter_dynamic <- renderUI({
    req(values$system_ready)
    
    tryCatch({
      available_data <- get_filtered_fire_data_for_filters()
      
      if (nrow(available_data) == 0) return(NULL)
      
      fuel_types <- sort(unique(available_data$attr_PrimaryFuelModel[
        !is.na(available_data$attr_PrimaryFuelModel) & 
          available_data$attr_PrimaryFuelModel != ""]))
      
      if (length(fuel_types) == 0) {
        return(div(style = "font-size: 12px; color: #000000;", "No fuel type data available"))
      }
      
      selectInput("fuel_type_filter", 
                  label = "Fuel Model:",
                  choices = c("All fuel types" = "", fuel_types),
                  selected = "",
                  multiple = TRUE)
      
    }, error = function(e) {
      div(class = "alert alert-warning", style = "padding: 5px; font-size: 11px;",
          "Error loading fuel types: ", e$message)
    })
  })
  
  # Fuel Category Filter
  output$fuel_category_filter_dynamic <- renderUI({
    req(values$system_ready)
    
    tryCatch({
      available_data <- get_filtered_fire_data_for_filters()
      
      if (nrow(available_data) == 0) return(NULL)
      
      fuel_categories <- sort(unique(available_data$fuel_category[
        !is.na(available_data$fuel_category) & 
          available_data$fuel_category != ""]))
      
      if (length(fuel_categories) == 0) {
        return(div(style = "font-size: 12px; color: #000000;", "No fuel category data available"))
      }
      
      selectInput("fuel_category_filter",
                  label = "Fuel Category:",
                  choices = c("All categories" = "", fuel_categories),
                  selected = "",
                  multiple = TRUE)
      
    }, error = function(e) {
      div(class = "alert alert-warning", style = "padding: 5px; font-size: 11px;",
          "Error loading fuel categories: ", e$message)
    })
  })
  
  # Landowner Category Filter
  output$landowner_category_filter_dynamic <- renderUI({
    req(values$system_ready)
    
    tryCatch({
      available_data <- get_filtered_fire_data_for_filters()
      
      if (nrow(available_data) == 0) return(NULL)
      
      landowner_cats <- sort(unique(available_data$landowner_category[
        !is.na(available_data$landowner_category) & 
          available_data$landowner_category != ""]))
      
      if (length(landowner_cats) == 0) {
        return(div(style = "font-size: 12px; color: #000000;", "No landowner data available"))
      }
      
      selectInput("landowner_category_filter",
                  label = "Landowner Category:",
                  choices = c("All categories" = "", landowner_cats),
                  selected = "",
                  multiple = TRUE)
      
    }, error = function(e) {
      div(class = "alert alert-warning", style = "padding: 5px; font-size: 11px;",
          "Error loading landowner categories: ", e$message)
    })
  })
  
  # Landowner Type Filter
  output$landowner_type_filter_dynamic <- renderUI({
    req(values$system_ready)
    
    tryCatch({
      available_data <- get_filtered_fire_data_for_filters()
      
      if (nrow(available_data) == 0) return(NULL)
      
      landowner_types <- sort(unique(available_data$landowner_type[
        !is.na(available_data$landowner_type) & 
          available_data$landowner_type != ""]))
      
      if (length(landowner_types) == 0) {
        return(div(style = "font-size: 12px; color: #000000;", "No landowner type data available"))
      }
      
      selectInput("landowner_type_filter",
                  label = "Landowner Type:",
                  choices = c("All types" = "", landowner_types),
                  selected = "",
                  multiple = TRUE)
      
    }, error = function(e) {
      div(class = "alert alert-warning", style = "padding: 5px; font-size: 11px;",
          "Error loading landowner types: ", e$message)
    })
  })
  
  # Filters Summary
  output$filters_summary_dynamic <- renderUI({
    req(input$state_select, input$fire_intensity_select)
    
    active_filters <- list()
    
    if (!is.null(input$fuel_type_filter) && length(input$fuel_type_filter) > 0 && 
        !all(input$fuel_type_filter == "")) {
      active_filters$fuel_type <- paste("Fuel Model:", paste(input$fuel_type_filter, collapse = ", "))
    }
    
    if (!is.null(input$fuel_category_filter) && length(input$fuel_category_filter) > 0 && 
        !all(input$fuel_category_filter == "")) {
      active_filters$fuel_category <- paste("Fuel Category:", paste(input$fuel_category_filter, collapse = ", "))
    }
    
    if (!is.null(input$landowner_category_filter) && length(input$landowner_category_filter) > 0 && 
        !all(input$landowner_category_filter == "")) {
      active_filters$landowner_category <- paste("Landowner Category:", paste(input$landowner_category_filter, collapse = ", "))
    }
    
    if (!is.null(input$landowner_type_filter) && length(input$landowner_type_filter) > 0 && 
        !all(input$landowner_type_filter == "")) {
      active_filters$landowner_type <- paste("Landowner Type:", paste(input$landowner_type_filter, collapse = ", "))
    }
    
    if (length(active_filters) > 0) {
      div(class = "filter-summary",
          h6(icon("filter"), " Active Filters:"),
          lapply(active_filters, function(filter) {
            div(style = "margin: 2px 0; font-size: 11px;", filter)
          })
      )
    } else {
      div(class = "summary-box", style = "font-size: 12px; color: #000000;",
          icon("info-circle"), " No additional filters applied. All ", 
          input$fire_intensity_select, " intensity fires will be available.")
    }
  })
}

render_fire_selection_ui <- function(input, output, get_available_fire_events) {
  output$fire_event_dropdown_dynamic <- renderUI({
    req(input$state_select, input$fire_intensity_select)
    
    tryCatch({
      available_fires <- get_available_fire_events()
      
      if (nrow(available_fires) == 0) {
        div(class = "alert alert-warning",
            "No fires match the current filter criteria. Try adjusting your filters.")
      } else {
        fire_choices_with_info <- setNames(
          available_fires$attr_IncidentName,
          paste0(available_fires$attr_IncidentName, " (", 
                 format(round(available_fires$total_area), big.mark = ","), " acres, ",
                 available_fires$perimeter_count, " perimeters)")
        )
        
        # Multiple selection interface
        div(
          h6(paste("Choose from", nrow(available_fires), "available fires:")),
          p("Select multiple fires to analyze compound fire events!", 
            style = "font-size: 12px; color: #0066cc; font-weight: bold; margin-bottom: 10px;"),
          
          # Multiple selection with checkboxes
          checkboxGroupInput("fire_events",
                             label = NULL,
                             choices = fire_choices_with_info,
                             selected = NULL),
          
          # Quick selection helpers
          div(style = "margin-top: 10px;",
              fluidRow(
                column(6,
                       actionButton("select_all_fires", "Select All", 
                                    class = "btn-sm btn-outline-primary", 
                                    style = "width: 100%;")
                ),
                column(6,
                       actionButton("clear_fire_selection", "Clear All", 
                                    class = "btn-sm btn-outline-secondary", 
                                    style = "width: 100%;")
                )
              )
          )
        )
      }
    }, error = function(e) {
      div(class = "alert alert-danger",
          "Error loading fire events: ", e$message)
    })
  })
  
  output$selected_fire_summary_dynamic <- renderUI({
    req(input$fire_events)
    
    if (is.null(input$fire_events) ||
        length(input$fire_events) == 0) {
      return(NULL)
    }
    
    fire_data <- get_available_fire_events()
    
    if (is.null(fire_data) || nrow(fire_data) == 0) {
      div(class = "alert alert-warning", "Selected fire data not available.")
    } else {
      selected_fires_info <- fire_data %>%
        filter(attr_IncidentName %in% input$fire_events)
      
      if (nrow(selected_fires_info) > 0) {
        total_area <- sum(selected_fires_info$total_area)
        total_perimeters <- sum(selected_fires_info$perimeter_count)
        num_fires <- length(input$fire_events)
        
        div(
          # Fire selection summary
          div(
            class = "summary-box",
            style = "background: #e1f5fe; border-color: #b3e5fc;",
            h6(
              icon("fire-alt"),
              " Selected Fires: ",
              strong(num_fires),
              if (num_fires == 1)
                " fire"
              else
                " fires"
            ),
            
            # Show individual fires if reasonable number
            if (num_fires <= 4) {
              div(
                h6("Fire Events:", style = "font-size: 12px; margin: 10px 0 5px 0;"),
                lapply(input$fire_events, function(fire_name) {
                  fire_info <- selected_fires_info %>% filter(attr_IncidentName == fire_name)
                  if (nrow(fire_info) > 0) {
                    div(
                      style = "font-size: 11px; margin: 2px 0; padding-left: 10px;",
                      "ðŸ”¥ ",
                      strong(fire_name),
                      ": ",
                      format(round(fire_info$total_area[1]), big.mark = ","),
                      " acres"
                    )
                  }
                })
              )
            },
            
            fluidRow(column(
              6,
              tags$ul(
                style = "font-size: 12px; margin: 5px 0; padding-left: 15px;",
                tags$li("State: ", strong(
                  tools::toTitleCase(input$state_select)
                )),
                tags$li("Intensity: ", strong(input$fire_intensity_select)),
                tags$li("Combined Area: ", strong(format(
                  round(total_area), big.mark = ","
                )), " acres")
              )
            ), column(
              6,
              tags$ul(
                style = "font-size: 12px; margin: 5px 0; padding-left: 15px;",
                tags$li("Fire Events: ", strong(num_fires)),
                tags$li("Total Perimeters: ", strong(total_perimeters)),
                tags$li(
                  "Analysis Type: ",
                  strong(if (num_fires == 1)
                    "Single Fire"
                    else
                      "ðŸ”¥ Compound Event")
                )
              )
            )),
            
            # Special note for compound events
            if (num_fires > 1) {
              div(
                style = "margin-top: 10px; padding: 8px; background: #fff8e1; border-radius: 4px; border: 1px solid #ffecb3;",
                icon("info-circle"),
                " ",
                strong("Compound Fire Analysis:"),
                " This will analyze the combined cascading effects and topological impact of multiple simultaneous fire events."
              )
            }
          ),
          
          div(
            style = "margin-top: 15px; text-align: center;",
            actionButton(
              "proceed_to_analysis",
              paste("Continue with", num_fires, if (num_fires == 1)
                "Fire"
                else
                  "Fires"),
              class = "btn-success btn-lg",
              style = "width: 100%; padding: 12px; font-size: 16px; font-weight: bold;",
              icon = icon("arrow-right")
            ),
            
            p(
              style = "font-size: 11px; color: #000000; margin-top: 8px;",
              "Ready to proceed? Click above to start the analysis workflow."
            )
          )
        )
      }
    }
  })
}

render_analysis_options_ui <- function(input, output, values, selected_fire) {
  
  output$cascade_summary_dynamic <- renderUI({
    req(values$enhanced_cascade_results)
    
    result <- values$enhanced_cascade_results
    metrics <- result$metrics
    
    if (nrow(metrics) == 0) return(NULL)
    
    total_fire_affected <- sum(metrics$fire_affected)
    total_deenergized <- sum(metrics$deenergized)
    max_cascade_ratio <- max(metrics$cascade_ratio)
    final_buses <- tail(metrics$vertices_remaining, 1)
    
    div(
      h6(icon("bolt"), " Cascade Simulation Results"),
      fluidRow(
        column(6,
               tags$ul(style = "font-size: 12px; margin: 0; padding-left: 15px;",
                       tags$li("Fire-affected: ", strong(total_fire_affected)),
                       tags$li("Cascade failures: ", strong(total_deenergized))
               )
        ),
        column(6,
               tags$ul(style = "font-size: 12px; margin: 0; padding-left: 15px;",
                       tags$li("Max amplification: ", strong(round(max_cascade_ratio, 2)), "x"),
                       tags$li("Final grid: ", strong(final_buses), " buses")
               )
        )
      )
    )
  })
  
  # TDA Controls
  output$tda_controls_dynamic <- renderUI({
    req(selected_fire())
    
    fire_data <- selected_fire()
    unique_fires <- length(unique(fire_data$attr_IncidentName))
    is_compound <- unique_fires > 1
    
    if (is_compound) {
      analysis_title <- paste("Complete Grid Topology Analysis (Compound Event:", unique_fires, "Fires)")
      analysis_description <- paste("Analyze before/after grid topology changes due to compound wildfire impact from", 
                                    unique_fires, "simultaneous fires using localized TDA.")
    } else {
      analysis_title <- "Complete Grid Topology Analysis"
      analysis_description <- "Analyze before/after grid topology changes due to wildfire impact using localized TDA."
    }
    
    div(
      h6(analysis_title),
      p(analysis_description, 
        style = "font-size: 12px; color: #000000; margin-bottom: 15px;"),
      
      fluidRow(
        column(12,
               numericInput("proximity_km", "Analysis Area Radius (km):", 
                            value = 5, min = 1, max = 50, step = 1)
        )
      ),
      
      div(style = "margin: 10px 0; padding: 8px; background: #f8f9fa; border-radius: 4px;",
          h6(style = "margin: 0; font-size: 12px; color: #495057;", 
             if (is_compound) "Compound Fire Analysis includes:" else "Analysis includes:"),
          tags$ul(style = "font-size: 11px; margin: 5px 0 0 0; padding-left: 20px; color: #6c757d;",
                  if (is_compound) {
                    list(
                      tags$li("Combined impact area topology (before compound fire)"),
                      tags$li("Full grid cascade simulation (compound fire effects)"),
                      tags$li("Combined impact area topology (after compound cascade)"),
                      tags$li("Topological change quantification (compound disturbance)")
                    )
                  } else {
                    list(
                      tags$li("Local area topology (before fire)"),
                      tags$li("Full grid cascade simulation"),
                      tags$li("Local area topology (after cascade)"),
                      tags$li("Topological change quantification")
                    )
                  }
          )
      ),
      
      br(),
      uiOutput("tda_analysis_button_dynamic")
    )
  })
  
  # Affected Buses Preview
  output$affected_buses_preview_dynamic <- renderUI({
    req(selected_fire(), input$proximity_km)
    
    tryCatch({
      fire_data <- selected_fire()
      affected_result <- find_buses_near_wildfire(fire_data, buses_sf, input$proximity_km)
      
      if (length(affected_result$affected_buses) == 0) {
        div(class = "alert alert-warning", style = "padding: 8px; font-size: 12px;",
            icon("exclamation-triangle"), 
            " No buses found within ", input$proximity_km, "km of this fire.")
      } else {
        bus_ids <- affected_result$affected_buses
        bus_range <- paste(min(bus_ids), "-", max(bus_ids))
        
        div(class = "summary-box", style = "background: #d4edda; border-color: #c3e6cb;",
            h6(icon("bolt"), " Impact Detection Results"),
            fluidRow(
              column(6,
                     tags$ul(style = "font-size: 11px; margin: 5px 0; padding-left: 15px;",
                             tags$li("Direct: ", strong(length(affected_result$direct_contact))),
                             tags$li("Proximity: ", strong(length(affected_result$proximity_contact)))
                     )
              ),
              column(6,
                     tags$ul(style = "font-size: 11px; margin: 5px 0; padding-left: 15px;",
                             tags$li("Total: ", strong(length(affected_result$affected_buses))),
                             tags$li("Range: ", strong(bus_range))
                     )
              )
            )
        )
      }
    }, error = function(e) {
      div(class = "alert alert-danger", style = "padding: 8px; font-size: 12px;",
          "Error detecting affected buses: ", e$message)
    })
  })
  
  # TDA Analysis Button
  output$tda_analysis_button_dynamic <- renderUI({
    req(selected_fire())
    
    tryCatch({
      fire_data <- selected_fire()
      fire_names <- unique(fire_data$attr_IncidentName)
      unique_fires <- length(fire_names)
      is_compound <- unique_fires > 1
      
      if (length(fire_names) == 0 || any(fire_names == "")) {
        div(
          actionButton("run_wildfire_tda", "No Fire Selected", 
                       class = "btn-warning", style = "width: 100%;", disabled = TRUE),
          div(style = "font-size: 11px; color: #000000; margin-top: 5px; text-align: center;",
              "Please select a valid fire event first")
        )
      } else {
        # Check if we have the required data
        fire_polygons <- nrow(fire_data)
        
        if (fire_polygons == 0) {
          div(
            actionButton("run_wildfire_tda", "No Fire Data Available", 
                         class = "btn-warning", style = "width: 100%;", disabled = TRUE),
            div(style = "font-size: 11px; color: #000000; margin-top: 5px; text-align: center;",
                "Selected fire has no polygon data")
          )
        } else {
          # Ready to run analysis
          if (is_compound) {
            button_text <- paste0("Run Compound Analysis: ", unique_fires, " Fires")
            if (nchar(button_text) > 35) {
              button_text <- paste0("Run Compound Analysis (", unique_fires, " fires)")
            }
            
            detail_text <- paste0("Analyze ", fire_polygons, " polygons from ", unique_fires, " simultaneous fires")
          } else {
            button_text <- paste0("Run Complete Analysis: ", fire_names[1])
            if (nchar(button_text) > 35) {
              button_text <- paste0("Run Analysis: ", substr(fire_names[1], 1, 15), "...")
            }
            
            detail_text <- paste0("Analyze ", fire_polygons, " fire polygons")
          }
          
          div(
            actionButton("run_wildfire_tda", button_text, 
                         class = "btn-success", style = "width: 100%;"),
            div(style = "font-size: 11px; color: #666; margin-top: 5px; text-align: center;",
                detail_text)
          )
        }
      }
    }, error = function(e) {
      div(
        actionButton("run_wildfire_tda", "Error in Fire Data", 
                     class = "btn-danger", style = "width: 100%;", disabled = TRUE),
        div(style = "font-size: 11px; color: #666; margin-top: 5px; text-align: center;",
            "Check fire data compatibility")
      )
    })
  })
  
  # TDA Results
  output$tda_results_dynamic <- renderUI({
    req(values$tda_results)
    
    result <- values$tda_results
    
    if (!result$success) {
      div(class = "alert alert-danger", style = "padding: 8px; font-size: 12px;",
          icon("times-circle"), " Analysis failed: ", result$error)
    } else {
      
      # Determine if this was a compound event
      is_compound <- if (!is.null(result$is_compound_event)) result$is_compound_event else FALSE
      fire_count <- if (!is.null(result$fire_count)) result$fire_count else 1
      
      if (is_compound) {
        analysis_type_text <- paste("Compound Event Analysis (", fire_count, " fires)")
        fire_display_text <- result$compound_fire_names
        analysis_description <- "Combined cascading effects and topological impact of multiple simultaneous fires"
      } else {
        analysis_type_text <- "Single Fire Analysis"  
        fire_display_text <- result$fire_name
        analysis_description <- "Before/after topological comparison from single fire impact"
      }
      
      div(class = "summary-box", style = "background: #d1ecf1; border-color: #bee5eb;",
          h6(icon("check-circle"), " Complete TDA Analysis Successful"),
          
          # Enhanced header for compound events
          if (is_compound) {
            div(style = "background: #fff3e0; padding: 8px; border-radius: 4px; margin: 8px 0; border-left: 4px solid #ff9800;",
                h6(style = "margin: 0; color: #e65100;", 
                   icon("fire-alt"), " COMPOUND FIRE EVENT ANALYSIS"),
                p(style = "margin: 4px 0 0 0; font-size: 11px; color: #bf360c;",
                  "Multi-fire simultaneous impact topology analysis completed"))
          },
          
          fluidRow(
            column(6,
                   tags$ul(style = "font-size: 11px; margin: 5px 0; padding-left: 15px;",
                           tags$li("Fire(s): ", strong(fire_display_text)),
                           tags$li("Type: ", strong(analysis_type_text))
                   )
            ),
            column(6,
                   tags$ul(style = "font-size: 11px; margin: 5px 0; padding-left: 15px;",
                           tags$li("Radius: ", strong(result$analysis_params$analysis_radius_km), " km"),
                           tags$li("Status: ", strong("Complete"))
                   )
            )
          ),
          
          div(style = "margin-top: 8px; font-size: 11px; color: #000000;",
              "", analysis_description),
          
          div(style = "margin-top: 5px; font-size: 11px; color: #000000;",
              "Detailed results saved to outputs directory"),
          
          if (!is.null(result$report_path)) {
            div(style = "margin-top: 8px; text-align: center;",
                actionButton("open_results_folder", "View Results Folder", 
                             class = "btn-sm btn-info", 
                             onclick = paste0("window.open('file://", dirname(result$report_path), "')"))
            )
          },
          
          div(style = "text-align: center; margin-top: 8px; font-size: 11px; color: #000000;",
              if (is_compound) {
                "Complete compound fire topology analysis including Wasserstein distance calculation."
              } else {
                "Full topology analysis including Wasserstein distance calculation."
              }
          )
      )
    }
  })
}