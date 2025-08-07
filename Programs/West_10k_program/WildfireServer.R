# =================================================================================================

# WildfireServer.R

# This file maintains all functionality while organizing code into logical modules

# Brandon Calvario

# =================================================================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shinyjs)

source("Global.R")
source("AttackAndCascade.R")
source("TopologicalDataWorkflowWF.R")

# ====================================================================
# 1. INITIALIZATION & SYSTEM MANAGEMENT
# ====================================================================
setup_reactive_values <- function() {
  # This function should be called within the server context
  # and should directly create the reactive values, not return a list
  
  # Create the main selected_state reactive
  selected_state <- reactiveVal(NULL)
  
  # Return the actual reactive values that can be used directly
  return(list(
    selected_state = selected_state
  ))
}
# Alternative approach - create reactive values directly in server function
initialize_server_state <- function() {
  reactiveValues(
    cascade_results = NULL,
    enhanced_cascade_results = NULL,
    tda_results = NULL,
    current_matrices = list(),
    initial_edges_sf = NULL,
    fire_impact_analysis = NULL,
    selected_fire_data = NULL,
    fire_affected_buses_history = list(),
    cascade_animation_data = NULL,
    system_ready = TRUE
  )
}

check_system_readiness <- function() {
  # Ensure leaflet is properly loaded
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet package is required but not available")
  }
  
  # Ensure color palettes are available
  if (!exists("bus_pal") || !exists("fire_intensity_pal")) {
    tryCatch({
      create_color_palettes()
      message("Color palettes recreated in server scope")
    }, error = function(e) {
      message("Warning: Could not create color palettes in server: ", e$message)
      # Create minimal palettes as fallback
      bus_pal <<- colorFactor(palette = c("red", "blue", "green", "gray"), 
                              domain = c("Generator", "Load", "Gen + Load", "Neither"))
      fire_intensity_pal <<- colorFactor(palette = c("yellow", "orange", "red"), 
                                         domain = c("Low", "Moderate", "High"))
    })
  }
  return(TRUE)
}

initialize_ui_state <- function() {
  # Process wildfire data with enhanced fields
  if (exists("wfigs_perimeters") && nrow(wfigs_perimeters) > 0) {
    tryCatch({
      wfigs_perimeters <<- process_loaded_wildfire_data(wfigs_perimeters)
      message("Wildfire data processed in server context")
    }, error = function(e) {
      message("Could not process wildfire data: ", e$message)
    })
  }
}

setup_error_handlers <- function(session) {
  # Add JavaScript error handling
  observeEvent(TRUE, {
    shinyjs::runjs("
      window.addEventListener('error', function(e) {
        console.warn('Client error:', e.message);
        return true;
      });
      
      window.safeInputValue = function(inputId, defaultValue) {
        try {
          if (typeof Shiny !== 'undefined' && 
              Shiny.shinyapp && 
              Shiny.shinyapp.$inputValues && 
              Shiny.shinyapp.$inputValues[inputId] !== undefined) {
            return Shiny.shinyapp.$inputValues[inputId];
          }
        } catch(e) {
          console.warn('Error accessing input ' + inputId + ':', e);
        }
        return defaultValue || '';
      };
    ")
  }, once = TRUE)
}

# ====================================================================
# 2. UI STATE MANAGEMENT
# ====================================================================

manage_ui_visibility <- function(input, values) {
  observeEvent(values$system_ready, {
    if (values$system_ready) {
      tryCatch({
        shinyjs::runjs("
          function safeInputCheck(inputId) {
            try {
              return (typeof Shiny !== 'undefined' && 
                      Shiny.shinyapp && 
                      Shiny.shinyapp.$inputValues && 
                      Shiny.shinyapp.$inputValues[inputId] && 
                      Shiny.shinyapp.$inputValues[inputId] !== '');
            } catch(e) {
              return false;
            }
          }
          
          function updateClearFiltersButton() {
            try {
              var hasFilters = safeInputCheck('fuel_type_filter') || 
                              safeInputCheck('fuel_category_filter') || 
                              safeInputCheck('landowner_category_filter') || 
                              safeInputCheck('landowner_type_filter');
              
              var clearBtn = document.getElementById('clear_all_filters');
              if (clearBtn) {
                clearBtn.style.display = hasFilters ? 'block' : 'none';
              }
            } catch(e) {
              console.warn('Error in updateClearFiltersButton:', e);
            }
          }
          
          function updateCascadeResults() {
            try {
              var cascadeContainer = document.getElementById('cascade-results-container');
              var runBtn = document.getElementById('run_cascade');
              
              if (cascadeContainer && runBtn) {
                var hasResults = runBtn.getAttribute('data-clicked') === 'true';
                var summaryBox = cascadeContainer.querySelector('.summary-box');
                if (summaryBox) {
                  summaryBox.style.display = hasResults ? 'block' : 'none';
                }
              }
            } catch(e) {
              console.warn('Error in updateCascadeResults:', e);
            }
          }
          
          setInterval(function() {
            updateClearFiltersButton();
            updateCascadeResults();
          }, 1000);
        ")
      }, error = function(e) {
        message("Error in UI visibility JavaScript: ", e$message)
      })
    }
  })
}

update_conditional_panels <- function(input, session) {
  observe({
    # Update clear filters button visibility
    has_filters <- (!is.null(input$fuel_type_filter) && length(input$fuel_type_filter) > 0 && !all(input$fuel_type_filter == "")) ||
      (!is.null(input$fuel_category_filter) && length(input$fuel_category_filter) > 0 && !all(input$fuel_category_filter == "")) ||
      (!is.null(input$landowner_category_filter) && length(input$landowner_category_filter) > 0 && !all(input$landowner_category_filter == "")) ||
      (!is.null(input$landowner_type_filter) && length(input$landowner_type_filter) > 0 && !all(input$landowner_type_filter == ""))
    
    if (has_filters) {
      shinyjs::show("clear_all_filters")
    } else {
      shinyjs::hide("clear_all_filters")
    }
  })
  
  # Update cascade results visibility
  observe({
    if (!is.null(input$run_cascade) && input$run_cascade > 0) {
      shinyjs::show("cascade-results-container")
    } else {
      shinyjs::hide("cascade-results-container")
    }
  })
}

handle_ui_transitions <- function(input, output, session) {
  observe({
    # Step 2 visibility
    step2_visible <- !is.null(input$state_select) && input$state_select != ""
    
    # Step 3 visibility  
    step3_visible <- step2_visible && !is.null(input$fire_intensity_select) && input$fire_intensity_select != ""
    
    # Step 4 visibility
    step4_visible <- step3_visible
    
    # Step 5 visibility
    step5_visible <- step4_visible && !is.null(input$fire_event) && input$fire_event != ""
    
    # You can add JavaScript to smoothly show/hide panels here if needed
    if (step5_visible) {
      shinyjs::removeClass("analysis-panel", "hidden")
    } else {
      shinyjs::addClass("analysis-panel", "hidden") 
    }
  })
}

reset_ui_state <- function(session) {
  updateSelectInput(session, "fire_intensity_select", selected = "")
  updateSelectInput(session, "fire_event", selected = "")
  updateSelectInput(session, "fuel_type_filter", selected = "")
  updateSelectInput(session, "fuel_category_filter", selected = "")
  updateSelectInput(session, "landowner_category_filter", selected = "")
  updateSelectInput(session, "landowner_type_filter", selected = "")
}

validate_ui_readiness <- function(input) {
  list(
    state_ready = !is.null(input$state_select) && input$state_select != "",
    intensity_ready = !is.null(input$fire_intensity_select) && input$fire_intensity_select != "",
    fire_ready = !is.null(input$fire_event) && input$fire_event != ""
  )
}

# ====================================================================
# 3. REACTIVE VALUE MANAGEMENT
# ====================================================================

setup_cascade_reactives <- function(input, values, session) {
  observe({
    if (!is.null(values$cascade_results)) {
      max_steps <- length(values$cascade_results$graphs) - 1
      if (max_steps > 0) {
        updateSliderInput(session, "step", 
                          min = 1, max = max_steps, 
                          value = min(1, max_steps))
      }
    }
  })
}

setup_fire_data_reactives <- function(input) {
  reactive({
    # Get selected fires safely
    selected_fires <- NULL
    
    if (!is.null(input$fire_events) && length(input$fire_events) > 0) {
      selected_fires <- input$fire_events[!is.na(input$fire_events)]
    } else if (!is.null(input$fire_event) && input$fire_event != "") {
      selected_fires <- input$fire_event
    }
    
    if (is.null(selected_fires) || length(selected_fires) == 0) {
      return(NULL)
    }
    
    # Simple filtering without complex compound logic
    fire_data <- wfigs_perimeters %>%
      filter(attr_IncidentName %in% selected_fires)
    
    # Apply additional filters safely
    if (!is.null(input$state_select) && input$state_select != "") {
      fire_data <- fire_data %>%
        filter(tolower(attr_POOState) == tolower(input$state_select))
    }
    
    if (!is.null(input$fire_intensity_select) && input$fire_intensity_select != "") {
      fire_data <- fire_data %>%
        filter(fire_intensity == input$fire_intensity_select)
    }
    
    if (nrow(fire_data) == 0) return(NULL)
    
    # Add simple compound metadata
    fire_data <- fire_data %>%
      mutate(
        is_compound_event = length(selected_fires) > 1,
        compound_fire_count = length(selected_fires)
      )
    
    return(fire_data)
  })
}

setup_filter_reactives <- function(input) {
  # Get available fire events
  reactive({
    req(input$state_select, input$fire_intensity_select)
    
    filtered_fires <- wfigs_perimeters %>%
      filter(tolower(attr_POOState) == tolower(input$state_select),
             fire_intensity == input$fire_intensity_select)
    
    # Apply additional filters if they exist
    if (!is.null(input$fuel_type_filter) && length(input$fuel_type_filter) > 0 && 
        !all(input$fuel_type_filter == "")) {
      filtered_fires <- filtered_fires %>%
        filter(attr_PrimaryFuelModel %in% input$fuel_type_filter)
    }
    
    if (!is.null(input$fuel_category_filter) && length(input$fuel_category_filter) > 0 && 
        !all(input$fuel_category_filter == "")) {
      filtered_fires <- filtered_fires %>%
        filter(fuel_category %in% input$fuel_category_filter)
    }
    
    if (!is.null(input$landowner_category_filter) && length(input$landowner_category_filter) > 0 && 
        !all(input$landowner_category_filter == "")) {
      filtered_fires <- filtered_fires %>%
        filter(landowner_category %in% input$landowner_category_filter)
    }
    
    if (!is.null(input$landowner_type_filter) && length(input$landowner_type_filter) > 0 && 
        !all(input$landowner_type_filter == "")) {
      filtered_fires <- filtered_fires %>%
        filter(landowner_type %in% input$landowner_type_filter)
    }
    
    fire_summary <- filtered_fires %>%
      group_by(attr_IncidentName) %>%
      summarise(
        total_area = sum(fire_acres, na.rm = TRUE),
        perimeter_count = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(total_area))
    
    return(fire_summary)
  })
}

setup_tda_reactives <- function(input, values, session) {
  observe({
    if (!is.null(values$cascade_results)) {
      max_steps <- length(values$cascade_results$graphs) - 1
      if (max_steps > 0) {
        updateSliderInput(session, "tda_analysis_step", 
                          min = 1, max = max_steps, 
                          value = min(ifelse(is.null(input$tda_analysis_step), 1, input$tda_analysis_step), max_steps))
      }
    }
  })
}

setup_map_reactives <- function(input, values, selected_fire) {
  
  # Fire color mode observer
  observeEvent(input$fire_color_mode, {
    if (is.null(input$fire_color_mode)) return()
    # Map update will be triggered by the observer in setup_map_observers
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
  
  # Map bounds reactive
  map_bounds <- reactive({
    if (!is.null(selected_fire()) && nrow(selected_fire()) > 0) {
      bbox <- st_bbox(selected_fire())
      list(
        lng1 = bbox["xmin"], lat1 = bbox["ymin"],
        lng2 = bbox["xmax"], lat2 = bbox["ymax"]
      )
    } else {
      # Default western US bounds
      list(lng1 = -125, lat1 = 31, lng2 = -102, lat2 = 49)
    }
  })
  
  return(list(map_bounds = map_bounds))
}

# ====================================================================
# 4. INPUT VALIDATION & SAFETY
# ====================================================================

safe_input_access <- function(input_id, default_value = NULL) {
  tryCatch({
    if (!is.null(input[[input_id]])) {
      return(input[[input_id]])
    }
    return(default_value)
  }, error = function(e) {
    return(default_value)
  })
}
validate_input_exists <- function(input_id, input) {
  !is.null(input[[input_id]]) && input[[input_id]] != ""
}
check_input_readiness <- function(required_inputs, input) {
  all(sapply(required_inputs, function(x) validate_input_exists(x, input)))
}
get_input_with_fallback <- function(input_id, fallback, input) {
  if (validate_input_exists(input_id, input)) {
    return(input[[input_id]])
  }
  return(fallback)
}

# ====================================================================
# 5. DYNAMIC UI RENDERING
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
                      "🔥 ",
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
                      "🔥 Compound Event")
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

# ====================================================================
# 6. EVENT HANDLERS (INITIALIZATION SAFE)
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
        incProgress(0.8, detail = "Updating interface...")
        # Update slider
        max_steps <- min(length(result$graphs) - 1, cfg$simulation_steps)
        updateSliderInput(session, "step", min = 1, max = max_steps, value = 1)
        incProgress(1.0, detail = "Complete!")
        # Clean up connections after successful completion
        connection_count_after <- monitor_connections()
        if (connection_count_after > connection_count + 5) {
          message("Connection count increased significantly, cleaning up...")
          cleanup_parallel_resources()
        }
        # Success notification
        total_affected <- sum(sapply(result$buses_lost_per_step, length))
        final_buses <- vcount(result$graphs[[length(result$graphs)]])
        
        showNotification(
          div(
            h5("Cascade Complete!"),
            p("Fire: ", strong(fire_name)),
            p("Buffer: ", strong(input$buffer_km), " km"),
            p("Buses Affected: ", strong(total_affected)),
            p("Final Grid Size: ", strong(final_buses), " buses"),
            br(),
            div(style = "text-align: center;",
                actionButton("run_tda_after_cascade", "Run TDA Analysis",
                             class = "btn-success", 
                             onclick = "document.getElementById('run_wildfire_tda').click();")
            )
          ),
          type = "message",
          duration = 15,
          closeButton = TRUE
        )
        
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
                p("🔥 Phase 1: Combined multi-fire impact analysis"),
                p("⚡ Phase 2: Enhanced cascade simulation with debugging"),
                p("📊 Phase 3: Before/after topology comparison"),
                p("📈 Phase 4: Enhanced visualization with ggplot2"),
                p("🔍 Phase 5: Detailed Wasserstein distance calculation")
              )
            } else {
              div(
                p("🔥 Phase 1: Fire impact analysis with validation"),
                p("⚡ Phase 2: Enhanced cascade simulation"),
                p("📊 Phase 3: TDA topology comparison with debugging"),
                p("📈 Phase 4: Advanced visualization generation"),
                p("🔍 Phase 5: Wasserstein distance with logging")
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
        
        # Enhanced success notification
        showNotification(
          div(
            h5(paste("Enhanced TDA Analysis Complete!", 
                     if(is_compound_event) "(COMPOUND EVENT)" else "")),
            
            div(style = "background: #e8f5e8; padding: 8px; border-radius: 4px; margin: 8px 0;",
                if (is_compound_event) {
                  div(
                    p(style = "margin: 0; font-weight: bold;", 
                      "🔥 Compound Fire Event: ", length(fire_names), " fires analyzed"),
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
                       "✓ Features Before: ", analysis_results$before_features),
                     p(style = "margin: 2px 0; font-size: 12px;",
                       "✓ Features After: ", analysis_results$after_features)
              ),
              column(6,
                     p(style = "margin: 2px 0; font-size: 12px;",
                       "✓ Wasserstein Distance: ", round(analysis_results$wasserstein_distance, 4)),
                     p(style = "margin: 2px 0; font-size: 12px;",
                       "✓ Analysis Type: ", if(is_compound_event) "Compound" else "Single")
              )
            ),
            
            br(),
            div(style = "text-align: center;",
                p("📊 Enhanced plots available in RStudio Plots panel!", 
                  style = "color: #28a745; font-weight: bold;"),
                p("🔍 Detailed debugging information logged to console", 
                  style = "color: #17a2b8; font-size: 11px;")
            ),
            
            if (is_compound_event) {
              div(style = "margin-top: 8px; padding: 6px; background: #fff3e0; border-radius: 4px;",
                  p(style = "margin: 0; font-size: 11px; color: #e65100;",
                    "🔥 Enhanced compound fire analysis captures multi-fire cascading topology patterns"))
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
# ====================================================================
# 7. OBSERVER MANAGEMENT
# ====================================================================
setup_initialization_observers <- function(values) {
  observe({
    tryCatch({
      if (is.null(values$initial_edges_sf)) {
        values$initial_edges_sf <- prepare_edges_sf(graph_original, bus_info)
        if (!is.null(values$initial_edges_sf)) {
          message("Initial edges prepared: ", nrow(values$initial_edges_sf), " edges")
        } else {
          message("Could not prepare initial edges")
          values$initial_edges_sf <- st_sf(
            from_bus = integer(0),
            to_bus = integer(0),
            geometry = st_sfc(crs = 4326)
          )
        }
      }
    }, error = function(e) {
      message("Could not prepare initial edges: ", e$message)
      # Create empty sf object as fallback
      values$initial_edges_sf <- st_sf(
        from_bus = integer(0),
        to_bus = integer(0), 
        geometry = st_sfc(crs = 4326)
      )
    })
  })
}

prepare_edges_sf <- function(graph = NULL, bus_coord_data = NULL) {
  # Use provided graph or default to graph_original
  target_graph <- if (!is.null(graph)) graph else {
    if (exists("graph_original")) graph_original else NULL
  }
  
  # Use provided bus data or try defaults
  coord_data <- if (!is.null(bus_coord_data)) bus_coord_data else {
    if (exists("bus_info")) bus_info else if (exists("buses_sf")) buses_sf else NULL
  }
  
  if (is.null(target_graph)) {
    message("Graph not available for edge preparation")
    return(NULL)
  }
  
  if (is.null(coord_data)) {
    message("Bus coordinate data not available for edge preparation")
    return(NULL)
  }
  
  if (ecount(target_graph) == 0 || nrow(coord_data) == 0) {
    message("Graph has no edges or no bus coordinate data available")
    return(NULL)
  }
  
  tryCatch({
    # Handle different data types (sf vs data.frame)
    if (inherits(coord_data, "sf")) {
      # If it's sf data, extract coordinates and bus_i
      coords_matrix <- st_coordinates(coord_data)
      coords <- data.frame(
        bus_i = coord_data$bus_i,
        longitude = coords_matrix[, "X"],
        latitude = coords_matrix[, "Y"]
      ) %>%
        filter(!is.na(longitude), !is.na(latitude)) %>%
        distinct(bus_i, .keep_all = TRUE)
    } else {
      # Handle different column names for coordinates
      if ("longitude" %in% names(coord_data) && "latitude" %in% names(coord_data)) {
        coords <- coord_data %>% 
          select(bus_i, longitude, latitude) %>%
          filter(!is.na(longitude), !is.na(latitude)) %>%
          distinct(bus_i, .keep_all = TRUE)
      } else if ("lon" %in% names(coord_data) && "lat" %in% names(coord_data)) {
        coords <- coord_data %>% 
          select(bus_i, lon, lat) %>%
          rename(longitude = lon, latitude = lat) %>%
          filter(!is.na(longitude), !is.na(latitude)) %>%
          distinct(bus_i, .keep_all = TRUE)
      } else {
        message("Could not find coordinate columns in bus data")
        return(NULL)
      }
    }
    
    if (nrow(coords) == 0) {
      message("No valid coordinates found")
      return(NULL)
    }
    
    edges_df <- igraph::as_data_frame(target_graph, what = "edges") %>%
      transmute(
        from_bus = as.integer(from), 
        to_bus = as.integer(to)
      ) %>%
      distinct(from_bus, to_bus, .keep_all = TRUE) %>%
      left_join(coords, by = c("from_bus" = "bus_i"), relationship = "many-to-one") %>%
      rename(lon_from = longitude, lat_from = latitude) %>%
      left_join(coords, by = c("to_bus" = "bus_i"), relationship = "many-to-one") %>%
      rename(lon_to = longitude, lat_to = latitude) %>%
      filter(!is.na(lon_from), !is.na(lat_from), !is.na(lon_to), !is.na(lat_to))
    
    if (nrow(edges_df) == 0) {
      message("No edges with valid coordinates found")
      return(NULL)
    }
    
    edge_lines <- lapply(1:nrow(edges_df), function(i) {
      st_linestring(matrix(c(edges_df$lon_from[i], edges_df$lat_from[i], 
                             edges_df$lon_to[i], edges_df$lat_to[i]), 
                           ncol = 2, byrow = TRUE))
    })
    
    edges_sf <- st_sf(
      from_bus = edges_df$from_bus,
      to_bus = edges_df$to_bus,
      geometry = st_sfc(edge_lines, crs = 4326)
    )
    
    message("Successfully prepared ", nrow(edges_sf), " edge connections")
    return(edges_sf)
    
  }, error = function(e) {
    message("Error preparing edges: ", e$message)
    return(NULL)
  })
}

setup_data_observers <- function(input, values, session) {
  # Update analysis step slider when cascade completes
  observe({
    if (!is.null(values$cascade_results)) {
      max_steps <- length(values$cascade_results$graphs) - 1
      if (max_steps > 0) {
        updateSliderInput(session, "tda_analysis_step", 
                          min = 1, max = max_steps, 
                          value = min(ifelse(is.null(input$tda_analysis_step), 1, input$tda_analysis_step), max_steps))
      }
    }
  })
}

setup_ui_update_observers <- function(input, output, session) {
  # Fire color mode observer
  observeEvent(input$fire_color_mode, {
    if (is.null(input$fire_color_mode)) return()
    # Trigger map update when color mode changes
    # (Map update logic handled in map management)
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
}

setup_map_observers <- function(input, values, selected_fire, selected_state, session) {
  # Map click handler
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id) && click$id %in% cfg$western_states) {
      # Use the selected_state parameter passed to this function
      if (!is.null(selected_state)) {
        selected_state(click$id)
      }
      
      # Update the UI input
      if (!is.null(session)) {
        updateSelectInput(session, "state_select", selected = click$id)
        
        # Reset downstream selections
        updateSelectInput(session, "fire_intensity_select", selected = "")
        updateSelectInput(session, "fire_event", selected = "")
      }
    }
  })
  observe({
    req(values$system_ready)
    
    # Only proceed if fires should be shown
    if (is.null(input$show_fires) || !input$show_fires) {
      # If fires shouldn't be shown, clear fire groups
      leafletProxy("map") %>% 
        clearGroup("fire_perimeters") %>%
        clearGroup("fire_centers") %>%
        clearGroup("impact_zones")
      return()
    }
    
    # Only proceed if we have a selected fire
    fire_data <- tryCatch({
      selected_fire()
    }, error = function(e) {
      message("Error accessing selected fire in observer: ", e$message)
      return(NULL)
    })
    
    if (is.null(fire_data) || !inherits(fire_data, "sf") || nrow(fire_data) == 0) {
      # Clear fire groups if no data
      leafletProxy("map") %>% 
        clearGroup("fire_perimeters") %>%
        clearGroup("fire_centers") %>%
        clearGroup("impact_zones")
      return()
    }
    
    tryCatch({
      proxy <- leafletProxy("map") %>% 
        clearGroup("fire_perimeters") %>%
        clearGroup("fire_centers") %>%
        clearGroup("impact_zones")
      
      # Remove existing fire legends
      tryCatch({
        proxy <- proxy %>% 
          removeControl(layerId = "fire_legend") %>%
          removeControl(layerId = "fuel_legend") %>%
          removeControl(layerId = "landowner_legend")
      }, error = function(e) {
        # Ignore legend removal errors
      })
      
      # Validate and fix geometries
      if (any(!st_is_valid(fire_data))) {
        fire_data <- st_make_valid(fire_data)
      }
      
      unique_fires <- length(unique(fire_data$attr_IncidentName))
      is_compound_event <- unique_fires > 1
      
      # Get color mode safely
      color_mode <- tryCatch({
        if (!is.null(input$fire_color_mode)) {
          input$fire_color_mode
        } else {
          "intensity"
        }
      }, error = function(e) {
        "intensity"
      })
      
      fire_data <- fire_data %>%
        mutate(
          # Ensure required columns exist with defaults
          attr_IncidentName = ifelse(is.na(attr_IncidentName) | attr_IncidentName == "", 
                                     "Unknown Fire", as.character(attr_IncidentName)),
          fire_intensity = ifelse(is.na(fire_intensity) | fire_intensity == "", 
                                  "Unknown", as.character(fire_intensity)),
          fire_acres = ifelse(is.na(fire_acres), 0, as.numeric(fire_acres)),
          
          # Create popup text using case_when for proper vectorization
          popup_text = case_when(
            is_compound_event ~ paste0("Compound Fire Event<br>",
                                       "Fire: ", attr_IncidentName, "<br>",
                                       "Event fires: ", unique_fires, "<br>",
                                       "This fire area: ", round(fire_acres), " acres<br>",
                                       "Intensity: ", fire_intensity),
            TRUE ~ paste0("Fire: ", attr_IncidentName, "<br>",
                          "Intensity: ", fire_intensity, "<br>",
                          "Area: ", round(fire_acres), " acres")
          )
        )
      
      message("Adding fire data to map: ", nrow(fire_data), " polygons")
      message("Color mode: ", color_mode)
      message("Is compound event: ", is_compound_event)
      
      # Apply appropriate coloring with enhanced popup
      if (color_mode == "attr_PrimaryFuelModel" && exists("fuel_category_pal")) {
        tryCatch({
          if (!"fuel_category" %in% names(fire_data)) {
            message("fuel_category column not found, using default coloring")
            color_mode <- "intensity"
          } else {
            proxy <- proxy %>%
              addPolygons(
                data = fire_data,
                group = "fire_perimeters",
                color = ~fuel_category_pal(fuel_category), 
                weight = if (is_compound_event) 3 else 2,
                fillOpacity = if (is_compound_event) 0.5 else 0.4,
                popup = ~popup_text
              ) %>%
              addLegend("topleft", 
                        pal = fuel_category_pal, 
                        values = fire_data$fuel_category, 
                        title = if (is_compound_event) "Fuel Category (Compound)" else "Fuel Category",
                        layerId = "fuel_legend")
            message("Added fuel category polygons to map")
            return()
          }
        }, error = function(e) {
          message("Error adding fuel category polygons: ", e$message)
          color_mode <- "intensity"
        })
      }
      
      if (color_mode == "attr_POOLandownerCategory" && exists("attr_POOLandownerCategory_pal")) {
        tryCatch({
          if (!"landowner_category" %in% names(fire_data)) {
            message("landowner_category column not found, using default coloring")
            color_mode <- "intensity"
          } else {
            proxy <- proxy %>%
              addPolygons(
                data = fire_data,
                group = "fire_perimeters", 
                color = ~attr_POOLandownerCategory_pal(landowner_category),
                weight = if (is_compound_event) 3 else 2,
                fillOpacity = if (is_compound_event) 0.5 else 0.4,
                popup = ~popup_text
              ) %>%
              addLegend("topleft",
                        pal = attr_POOLandownerCategory_pal,
                        values = fire_data$landowner_category,
                        title = if (is_compound_event) "Landowner (Compound)" else "Landowner Category", 
                        layerId = "landowner_legend")
            message("Added landowner category polygons to map")
            return()
          }
        }, error = function(e) {
          message("Error adding landowner category polygons: ", e$message)
          color_mode <- "intensity"
        })
      }
      
      # Default intensity coloring with compound event styling
      if (exists("fire_intensity_pal")) {
        tryCatch({
          proxy <- proxy %>%
            addPolygons(
              data = fire_data,
              group = "fire_perimeters",
              color = ~fire_intensity_pal(fire_intensity), 
              weight = if (is_compound_event) 3 else 2,
              fillOpacity = if (is_compound_event) 0.5 else 0.4,
              popup = ~popup_text
            ) %>%
            addLegend("topleft", 
                      pal = fire_intensity_pal, 
                      values = fire_data$fire_intensity, 
                      title = if (is_compound_event) "Fire Intensity (Compound)" else "Fire Intensity",
                      layerId = "fire_legend")
          message("Added fire intensity polygons to map")
        }, error = function(e) {
          message("Error adding intensity polygons: ", e$message)
        })
      } else {
        message("Warning: fire_intensity_pal palette not found")
      }
      
    }, error = function(e) {
      message("Error in fire display observer: ", e$message)
    })
  })
  # Map layer visibility control
  observe({
    proxy <- leafletProxy("map")
    groups_to_show <- c()
    groups_to_hide <- c()
    
    if (!is.null(input$show_buses) && input$show_buses) groups_to_show <- c(groups_to_show, "initial_buses")
    else groups_to_hide <- c(groups_to_hide, "initial_buses")
    
    if (!is.null(input$show_lines) && input$show_lines) groups_to_show <- c(groups_to_show, "initial_lines")
    else groups_to_hide <- c(groups_to_hide, "initial_lines")
    
    if (!is.null(input$show_fires) && input$show_fires) groups_to_show <- c(groups_to_show, "fire_perimeters")
    else groups_to_hide <- c(groups_to_hide, "fire_perimeters")
    
    if (!is.null(input$show_fire_centers) && input$show_fire_centers) groups_to_show <- c(groups_to_show, "fire_centers")
    else groups_to_hide <- c(groups_to_hide, "fire_centers")
    
    if (!is.null(input$show_impact_zones) && input$show_impact_zones) groups_to_show <- c(groups_to_show, "impact_zones")
    else groups_to_hide <- c(groups_to_hide, "impact_zones")
    
    for (group in groups_to_show) proxy %>% showGroup(group)
    for (group in groups_to_hide) proxy %>% hideGroup(group)
  })
}


setup_analysis_observers <- function(input, values, session = NULL) {
  observe({
    req(values$cascade_results, input$step)
    
    step <- input$step
    cascade_result <- values$cascade_results
    
    # Get current graph state and failed components
    g_cur <- cascade_result$graphs[[step + 1]] # Graphs list is 1-indexed by step+1
    active_buses <- as.numeric(V(g_cur)$name)
    buses_lost_so_far <- unique(unlist(cascade_result$buses_lost_per_step[1:step]))
    
    # Prepare visualization data
    buses_active_sf <- buses_sf %>% filter(bus_i %in% active_buses)
    
    fire_points <- if (step <= length(cascade_result$fire_points_list)) {
      points_data <- cascade_result$fire_points_list[[step]]
      
      # Ensure fire points data has the expected structure
      if (!is.null(points_data) && nrow(points_data) > 0) {
        # Check if compound fire information is available
        if ("is_compound_impact" %in% names(points_data)) {
          message("Step ", step, ": Processing fire points with compound event information")
          points_data
        } else {
          # Add compound event information if missing
          points_data %>%
            mutate(
              is_compound_impact = FALSE,
              affecting_fire_count = 1,
              affecting_fire_names = if("attr_IncidentName" %in% names(.)) attr_IncidentName else "Unknown"
            )
        }
      } else {
        buses_sf[0, ] %>%
          mutate(
            impact_type = character(0),
            fire_step = integer(0),
            is_compound_impact = logical(0),
            affecting_fire_count = integer(0),
            affecting_fire_names = character(0)
          )
      }
    } else {
      buses_sf[0, ] %>%
        mutate(
          impact_type = character(0),
          fire_step = integer(0),
          is_compound_impact = logical(0),
          affecting_fire_count = integer(0),
          affecting_fire_names = character(0)
        )
    }
    
    # Get all buses that have failed up to this point
    buses_failed_sf <- buses_sf %>% filter(bus_i %in% buses_lost_so_far)
    
    edges_active_sf <- prepare_edges_sf(g_cur, bus_info)
    
    # Find edges connected to at least one failed bus
    failed_edges_df <- igraph::as_data_frame(graph_original, what = "edges") %>%
      filter(from %in% buses_lost_so_far | to %in% buses_lost_so_far)
    failed_edges_graph <- graph_from_data_frame(failed_edges_df, directed = FALSE)
    edges_failed_sf <- prepare_edges_sf(failed_edges_graph, bus_info)
    
    #Create appropriate fire icons based on compound status
    if (nrow(fire_points) > 0 && any(fire_points$is_compound_impact, na.rm = TRUE)) {
      # Compound fire icon (larger, different color)
      fireIcon <- makeIcon(
        iconUrl = "https://img.icons8.com/plasticine/100/ff6b35/fire-element.png",
        iconWidth = 35, iconHeight = 35,
        iconAnchorX = 17, iconAnchorY = 17
      )
      message("Using compound fire icons for step ", step)
    } else {
      # Standard fire icon
      fireIcon <- makeIcon(
        iconUrl = "https://img.icons8.com/plasticine/100/000000/fire-element.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 15, iconAnchorY = 15
      )
    }
    
    # Update map
    proxy <- leafletProxy("map") %>% 
      clearGroup("simulation") %>%
      clearGroup("failed_elements") %>%
      hideGroup("initial_buses") %>%
      hideGroup("initial_lines")
    
    # Add FAILED branches (in red)
    if (input$show_lines && !is.null(edges_failed_sf) && nrow(edges_failed_sf) > 0) {
      proxy <- proxy %>%
        addPolylines(
          data = edges_failed_sf, 
          group = "failed_elements",
          color = "#E74C3C",
          weight = 2, 
          opacity = 0.8
        )
    }
    
    # Add ACTIVE branches
    if (input$show_lines && !is.null(edges_active_sf) && nrow(edges_active_sf) > 0) {
      proxy <- proxy %>%
        addPolylines(
          data = edges_active_sf, 
          group = "simulation",
          color = "#0066CC", 
          weight = 2, 
          opacity = 0.7
        )
    }
    
    # Add FAILED buses (as gray 'X's)
    if (input$show_buses && nrow(buses_failed_sf) > 0) {
      proxy <- proxy %>%
        addMarkers(
          data = buses_failed_sf,
          group = "failed_elements",
          icon = makeIcon(iconUrl = "https://img.icons8.com/ios-filled/50/a0a0a0/delete-sign.png", 
                          iconWidth=12, iconHeight=12),
          popup = ~paste0("Bus: ", bus_i, "<br>Status: FAILED")
        )
    }
    
    # Add ACTIVE buses
    if (input$show_buses && nrow(buses_active_sf) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = buses_active_sf,
          group = "simulation",
          radius = 5, 
          color = ~bus_pal(bus_type), 
          fillOpacity = 0.8,
          stroke = TRUE, weight = 1,
          popup = ~paste0("Bus: ", bus_i, "<br>Type: ", bus_type, "<br>Status: Active")
        )
    }
    
    if (nrow(fire_points) > 0) {
      
      message("Adding ", nrow(fire_points), " fire icons for step ", step)
      
      # Create enhanced popup text for fire-affected buses
      fire_points_with_popup <- fire_points %>%
        mutate(
          popup_text = case_when(
            is_compound_impact ~ paste0(
              "Bus: ", bus_i, 
              "<br>Impact: Compound Fire Event",
              "<br>Step: ", fire_step,
              "<br>Fire Count: ", affecting_fire_count,
              "<br>Fires: ", affecting_fire_names,
              "<br>Impact Type: ", tools::toTitleCase(impact_type)
            ),
            TRUE ~ paste0(
              "Bus: ", bus_i, 
              "<br>Impact: Fire",
              "<br>Step: ", fire_step,
              "<br>Impact Type: ", tools::toTitleCase(impact_type)
            )
          )
        )
      
      proxy <- proxy %>%
        addMarkers(
          data = fire_points_with_popup,
          group = "failed_elements",
          icon = fireIcon,
          popup = ~popup_text
        )
      
      # Log fire icon placement for debugging
      compound_count <- sum(fire_points$is_compound_impact, na.rm = TRUE)
      if (compound_count > 0) {
        message("Placed ", compound_count, " compound fire icons and ", 
                nrow(fire_points) - compound_count, " standard fire icons")
      } else {
        message("Placed ", nrow(fire_points), " standard fire icons")
      }
    } else {
      message("No fire points to display for step ", step)
    }
  })
}
# ====================================================================
# 8. MAP MANAGEMENT
# ====================================================================
initialize_base_map <- function(output, values) {
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = states_sf, 
        layerId = ~ID, 
        fillOpacity = 0.1, 
        color = "#555", 
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 2,
          fillOpacity = 0.2,
          bringToFront = TRUE
        )
      ) %>%
      fitBounds(-125, 31, -102, 49)
    
    map <- map %>%
      addLegend("bottomright", 
                pal = bus_pal, 
                values = buses_sf$bus_type, 
                title = "Bus Type",
                layerId = "bus_legend")
    
    if (!is.null(values$initial_edges_sf) && nrow(values$initial_edges_sf) > 0) {
      map <- map %>%
        addPolylines(
          data = values$initial_edges_sf,
          group = "initial_lines",
          color = "#4169E1",
          weight = 1,
          opacity = 0.5
        )
    }
    
    if (nrow(buses_sf) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = buses_sf,
          group = "initial_buses",
          radius = 4,
          color = ~bus_pal(bus_type),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0("Bus: ", bus_i, "<br>",
                          "Type: ", bus_type, "<br>",
                          "Voltage: ", round(vm, 3), " p.u.<br>",
                          "Load: ", round(load_mw, 1), " MW<br>",
                          "Generation: ", round(total_gen, 1), " MW")
        )
    }
    
    map
  })
}

update_fire_display <- function() {
  # Handled in observers
}

update_bus_display <- function() {
  # Handled in observers
}

update_simulation_display <- function() {
  # Handled in observers
}

handle_map_interactions <- function() {
  # Handled in observers
}


safe_legend_management <- function() {
  safe_remove_legend <- function(map_proxy, layerId) {
    tryCatch({
      if (!is.null(layerId)) {
        map_proxy %>% removeControl(layerId = layerId)
      } else {
        map_proxy
      }
    }, error = function(e) {
      tryCatch({
        map_proxy %>% leaflet::removeControl(layerId = layerId)
      }, error = function(e2) {
        map_proxy
      })
    })
  }
  return(safe_remove_legend)
}

# ====================================================================
# 9. DATA PROCESSING
# ====================================================================

process_fire_data_safely <- function(fire_data) {
  if (is.null(fire_data) || nrow(fire_data) == 0) {
    return(NULL)
  }
  
  tryCatch({
    if (any(!st_is_valid(fire_data))) {
      fire_data <- st_make_valid(fire_data)
    }
    return(fire_data)
  }, error = function(e) {
    message("Error processing fire data: ", e$message)
    return(NULL)
  })
}

filter_fire_data <- function(fire_data, filters) {
  # Apply filters to fire data
  if (!is.null(filters$state)) {
    fire_data <- fire_data %>%
      filter(tolower(attr_POOState) == tolower(filters$state))
  }
  
  if (!is.null(filters$intensity)) {
    fire_data <- fire_data %>%
      filter(fire_intensity == filters$intensity)
  }
  
  if (!is.null(filters$fuel_type) && length(filters$fuel_type) > 0) {
    fire_data <- fire_data %>%
      filter(attr_PrimaryFuelModel %in% filters$fuel_type)
  }
  
  if (!is.null(filters$fuel_category) && length(filters$fuel_category) > 0) {
    fire_data <- fire_data %>%
      filter(fuel_category %in% filters$fuel_category)
  }
  
  if (!is.null(filters$landowner_category) && length(filters$landowner_category) > 0) {
    fire_data <- fire_data %>%
      filter(landowner_category %in% filters$landowner_category)
  }
  
  if (!is.null(filters$landowner_type) && length(filters$landowner_type) > 0) {
    fire_data <- fire_data %>%
      filter(landowner_type %in% filters$landowner_type)
  }
  
  return(fire_data)
}

validate_fire_selection <- function(fire_data) {
  if (is.null(fire_data) || nrow(fire_data) == 0) {
    return(list(valid = FALSE, message = "No fire data available"))
  }
  
  if (!inherits(fire_data, "sf")) {
    return(list(valid = FALSE, message = "Fire data is not spatial"))
  }
  
  return(list(valid = TRUE))
}

prepare_cascade_data <- function(cascade_results) {
  # Prepare cascade results for visualization
  if (is.null(cascade_results)) return(NULL)
  
  # Extract metrics and format for display
  metrics <- cascade_results$metrics
  if (nrow(metrics) == 0) return(NULL)
  
  return(metrics)
}

generate_analysis_matrices <- function(graph, affected_buses) {
  # Generate various matrices for analysis
  matrices <- list()
  
  # Power difference matrix
  if (exists("generate_post_cascade_power_matrix")) {
    matrices$power <- generate_post_cascade_power_matrix(graph, affected_buses)
  }
  
  # Adjacency matrix
  matrices$adjacency <- igraph::get.adjacency(graph, attr = "weight")
  
  return(matrices)
}

# ====================================================================
# 10. ANALYSIS EXECUTION
# ====================================================================

execute_cascade_simulation <- function(graph, buses_sf, fire_data, buffer_km, steps) {
  # Wrapper for cascade simulation with error handling
  tryCatch({
    result <- run_enhanced_fire_cascade(
      graph = graph,
      buses_sf = buses_sf,
      fire_data = fire_data,
      buffer_km = buffer_km,
      steps = steps
    )
    return(list(success = TRUE, result = result))
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

run_tda_analysis <- function(matrix_data, analysis_type) {
  # Wrapper for TDA analysis
  tryCatch({
    if (analysis_type == "perseus") {
      result <- run_perseus_analysis(matrix_data)
    } else {
      # Other TDA methods
      result <- NULL
    }
    return(result)
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

generate_reports <- function(cascade_results, tda_results, fire_info) {
  # Generate analysis reports
  report <- list(
    timestamp = Sys.time(),
    fire_info = fire_info,
    cascade_summary = summarize_cascade_results(cascade_results),
    tda_summary = summarize_tda_results(tda_results)
  )
  
  return(report)
}

export_results <- function(results, export_format) {
  # Export results in specified format
  if (export_format == "csv") {
    # Export to CSV
  } else if (export_format == "json") {
    # Export to JSON
  } else if (export_format == "shp") {
    # Export to shapefile
  }
}

# Helper functions
summarize_cascade_results <- function(results) {
  if (is.null(results)) return(NULL)
  
  metrics <- results$metrics
  summary <- list(
    total_steps = nrow(metrics),
    total_buses_lost = sum(metrics$total_lost),
    total_fire_affected = sum(metrics$fire_affected),
    total_cascade_failures = sum(metrics$deenergized),
    max_cascade_ratio = max(metrics$cascade_ratio),
    final_grid_size = tail(metrics$vertices_remaining, 1)
  )
  
  return(summary)
}

summarize_tda_results <- function(results) {
  if (is.null(results) || !results$success) return(NULL)
  
  diagram <- results$diagram
  summary <- list(
    total_features = nrow(diagram),
    features_by_dimension = table(diagram$Dimension),
    max_persistence = max(diagram$Death - diagram$Birth)
  )
  
  return(summary)
}

# ====================================================================
# OUTPUT FUNCTIONS
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
      
      # Fallback: create comparison plot if data is available
      if (!is.null(values$tda_results$before_features) && 
          !is.null(values$tda_results$after_features)) {
        
        comparison_data <- data.frame(
          State = c("Before", "After"),
          Features = c(values$tda_results$before_features, 
                       values$tda_results$after_features)
        )
        
        ggplot(comparison_data, aes(x = State, y = Features, fill = State)) +
          geom_col(alpha = 0.8) +
          scale_fill_manual(values = c("Before" = "#2E86AB", "After" = "#E63946")) +
          labs(
            title = "TDA Feature Comparison",
            subtitle = paste("Wasserstein Distance:", 
                             round(values$tda_results$wasserstein_distance, 4)),
            y = "Number of Features"
          ) +
          theme_minimal() +
          theme(legend.position = "none")
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
      
      # Fallback: create from persistence data
      if (!is.null(values$tda_results$after_features)) {
        diagram <- values$tda_results$persistence_data
      } else {
        diagram <- values$tda_results$diagram
      }
      
      if (is.null(diagram) || nrow(diagram) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No persistent features found") +
                 theme_minimal())
      }
      
      # Ensure correct column names
      if (ncol(diagram) >= 3) {
        colnames(diagram) <- c("Dimension", "Birth", "Death")
        diagram <- as.data.frame(diagram)
      }
      
      colors <- c("0" = "#2E86AB", "1" = "#E63946", "2" = "#2ca02c")
      
      p <- ggplot(diagram, aes(Birth, Death, color = factor(Dimension))) +
        geom_point(size = 3, alpha = 0.7) +
        geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed") +
        scale_color_manual(values = colors, name = "Dimension") +
        coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
        labs(
          title = "TDA Persistence Diagram",
          subtitle = paste("Features:", nrow(diagram)),
          x = "Birth (Normalized)",
          y = "Death (Normalized)"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      if (nrow(diagram) > 0) {
        top_features <- diagram %>%
          mutate(persistence = Death - Birth) %>%
          arrange(desc(persistence)) %>%
          head(3)
        
        if (nrow(top_features) > 0) {
          p <- p + 
            geom_point(data = top_features, 
                       shape = 1, size = 5, stroke = 2, color = "red", alpha = 0.8)
        }
      }
      
      return(p)
      
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("TDA Error:", e$message)) +
        theme_minimal()
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
      
      # Generate enhanced report
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

# ====================================================================
# MAIN SERVER FUNCTION
# ====================================================================

server <- function(input, output, session) {
  # 1. System readiness check
  tryCatch({
    check_system_readiness()
  }, error = function(e) {
    showNotification("System initialization error", type = "error")
    return()
  })
  
  # 2. Initialize server state
  values <- initialize_server_state()
  
  # 3. Setup reactive values
  selected_state <- reactiveVal(NULL)
  
  # 4. Setup UI state management
  tryCatch({
    initialize_ui_state()
    setup_error_handlers(session)
  }, error = function(e) {
    message("Warning: UI state setup error: ", e$message)
  })
  
  # 5. Setup reactive expressions
  selected_fire <- tryCatch({
    setup_fire_data_reactives(input)
  }, error = function(e) {
    message("Error setting up fire data reactives: ", e$message)
    reactive({ NULL })
  })
  
  get_available_fire_events <- tryCatch({
    setup_filter_reactives(input)
  }, error = function(e) {
    message("Error setting up filter reactives: ", e$message)
    reactive({ data.frame() })
  })
  
  # 6. Setup filter data reactive
  get_filtered_fire_data_for_filters <- reactive({
    req(values$system_ready)
    
    if (is.null(input$state_select) || input$state_select == "" ||
        is.null(input$fire_intensity_select) || input$fire_intensity_select == "" ||
        !exists("wfigs_perimeters")) {
      return(data.frame())
    }
    
    tryCatch({
      wfigs_perimeters %>%
        filter(tolower(attr_POOState) == tolower(input$state_select),
               fire_intensity == input$fire_intensity_select)
    }, error = function(e) {
      message("Error in get_filtered_fire_data_for_filters: ", e$message)
      return(data.frame())
    })
  })
  
  # 7. Setup additional reactive systems
  setup_cascade_reactives(input, values, session)
  setup_tda_reactives(input, values, session)
  map_reactives <- setup_map_reactives(input, values, selected_fire)
  
  # 8. Create has_active_filters output
  output$has_active_filters <- renderText({
    req(input)  # Ensure input is available
    
    # Check each filter type safely
    fuel_type_active <- !is.null(input$fuel_type_filter) && 
      length(input$fuel_type_filter) > 0 && 
      !all(input$fuel_type_filter == "")
    
    fuel_category_active <- !is.null(input$fuel_category_filter) && 
      length(input$fuel_category_filter) > 0 && 
      !all(input$fuel_category_filter == "")
    
    landowner_category_active <- !is.null(input$landowner_category_filter) && 
      length(input$landowner_category_filter) > 0 && 
      !all(input$landowner_category_filter == "")
    
    landowner_type_active <- !is.null(input$landowner_type_filter) && 
      length(input$landowner_type_filter) > 0 && 
      !all(input$landowner_type_filter == "")
    
    return(fuel_type_active || fuel_category_active || landowner_category_active || landowner_type_active)
  })
  outputOptions(output, "has_active_filters", suspendWhenHidden = FALSE)
  
  # 9. Setup legend removal helper
  safe_remove_legend <- safe_legend_management()
  
  # 10. Render Dynamic UI Components
  tryCatch({
    render_state_selection_ui(input, output)
    render_intensity_selection_ui(input, output)
    render_filter_options_ui(input, output, values, get_filtered_fire_data_for_filters)
    render_fire_selection_ui(input, output, get_available_fire_events)
    render_analysis_options_ui(input, output, values, selected_fire)
  }, error = function(e) {
    message("Error in UI rendering: ", e$message)
  })
  
  # 11. Setup Event Handlers
  tryCatch({
    handle_state_selection(input, session, selected_state)
    handle_intensity_selection(input, session)
    handle_filter_changes(input, session)
    handle_multiple_fire_selection(input, session, get_available_fire_events)
    handle_cascade_execution(input, values, selected_fire, session)
    handle_tda_analysis(input, values, selected_fire, session)
  }, error = function(e) {
    message("Error in event handler setup: ", e$message)
  })
  
  # 12. Setup UI Transitions
  handle_ui_transitions(input, output, session)
  update_conditional_panels(input, session)
  
  # 13. Setup Observers
  tryCatch({
    setup_initialization_observers(values)
    setup_data_observers(input, values, session)
    setup_ui_update_observers(input, output, session)
    setup_map_observers(input, values, selected_fire, selected_state, session)
    setup_analysis_observers(input, values, session)
  }, error = function(e) {
    message("Error in observer setup: ", e$message)
  })
  
  # 14. Initialize Map
  tryCatch({
    initialize_base_map(output, values)
  }, error = function(e) {
    message("Error initializing map: ", e$message)
  })
  
  # 15. Setup Output Renderers
  tryCatch({
    render_output_functions(output, values, input, selected_fire)
  }, error = function(e) {
    message("Error setting up output renderers: ", e$message)
  })
  
  # 16. Final UI visibility management
  manage_ui_visibility(input, values)
}

ui <- fluidPage(
  # Enable shinyjs
  useShinyjs(),
  
  titlePanel("Enhanced Wildfire Grid Resilience Explorer"),
  tags$head(tags$style(
    HTML(
      "
    /* Existing styles remain unchanged */
    .control-panel {
      background: rgba(255,255,255,0.95);
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      /* ADD these lines: */
      max-height: 750px;
      overflow-y: auto;
      position: fixed !important;
      z-index: 1000;
      }
      .selection-step {
        border-left: 4px solid #007bff;
        padding-left: 15px;
        margin-bottom: 20px;
        background: #f8f9fa;
        border-radius: 0 4px 4px 0;
        padding: 15px;
      }
      .selection-step.completed {
        border-left-color: #28a745;
        background: #d4edda;
      }
      .selection-step.active {
        border-left-color: #ffc107;
        background: #fff3cd;
      }
      .step-header {
        display: flex;
        align-items: center;
        margin-bottom: 10px;
      }
      .step-number {
        background: #007bff;
        color: white;
        border-radius: 50%;
        width: 25px;
        height: 25px;
        display: flex;
        align-items: center;
        justify-content: center;
        margin-right: 10px;
        font-size: 12px;
        font-weight: bold;
      }
      .step-number.completed {
        background: #28a745;
      }
      .step-number.active {
        background: #ffc107;
      }
      .summary-box {
        background: #e7f3ff;
        border: 1px solid #b3d9ff;
        border-radius: 4px;
        padding: 10px;
        margin: 10px 0;
        font-size: 13px;
      }
      .filter-group {
        background: #f1f3f4;
        border-radius: 4px;
        padding: 12px;
        margin-top: 10px;
      }
      .filter-summary {
        background: #fff3cd;
        border: 1px solid #ffeaa7;
        border-radius: 4px;
        padding: 8px;
        margin-top: 10px;
        font-size: 12px;
      }
      /* Hide elements that should appear dynamically */
      #clear-filters-container .btn {
        display: none;
      }
      #cascade-results-container .summary-box {
        display: none;
      }
          /* NEW: Ensure proper spacing for bottom sections */
    .results-section {
      margin-top: 30px;
      padding-top: 20px;
      border-top: 2px solid #e9ecef;
      clear: both;
    }

    /* NEW: Responsive adjustments */
    @media (max-width: 1200px) {
      .control-panel {
        position: relative !important;
        width: 100% !important;
        right: auto !important;
        top: auto !important;
        margin-bottom: 20px;
      }
    }

    /* NEW: Plot container spacing */
    .plot-container {
      margin-bottom: 30px;
      padding: 15px;
      background: #f8f9fa;
      border-radius: 8px;
    }
    "
    )
  ), tags$script(
    HTML(
      "
  Shiny.addCustomMessageHandler('updateProgress', function(data) {
    var progressBar = document.querySelector('#tda-progress .progress-bar');
    if (progressBar) {
      progressBar.style.width = data.progress + '%';
      progressBar.textContent = data.text;

      if (data.progress >= 100) {
        progressBar.classList.remove('progress-bar-animated');
        progressBar.classList.add('bg-success');
      }
    }
  });
"
    )
  ), tags$script(
    HTML(
      "
      // Global error handler for Shiny
      window.addEventListener('error', function(e) {
        console.warn('Client error:', e.message);
        // Don't let client errors break the app
        return true;
      });

      // Safe Shiny input checking
      window.safeInputValue = function(inputId, defaultValue) {
        try {
          if (typeof Shiny !== 'undefined' &&
              Shiny.shinyapp &&
              Shiny.shinyapp.$inputValues &&
              Shiny.shinyapp.$inputValues[inputId] !== undefined) {
            return Shiny.shinyapp.$inputValues[inputId];
          }
        } catch(e) {
          console.warn('Error accessing input ' + inputId + ':', e);
        }
        return defaultValue || '';
      };
        $(window).resize(function() {
    var windowWidth = $(window).width();
    var controlPanel = $('.control-panel');
    var mapContainer = $('#map').parent();

    if (windowWidth < 1200) {
      // Mobile/tablet layout
      controlPanel.css({
        'position': 'relative',
        'width': '100%',
        'right': 'auto',
        'top': 'auto',
        'margin-bottom': '20px'
      });
      mapContainer.css('margin-right', '0');
    } else {
      // Desktop layout
      controlPanel.css({
        'position': 'fixed',
        'width': '420px',
        'right': '10px',
        'top': '10px'
      });
      mapContainer.css('margin-right', '440px');
    }
  });

  // Trigger resize on load
  $(document).ready(function() {
    $(window).trigger('resize');
  });
"
    )
  )),
  # Main map
  div(
    style = "position: relative; margin-bottom: 20px;",
    # Map container with reserved space for control panel
    div(
      style = "margin-right: 440px;",
      # Leave space for 420px panel + 20px margin
      leafletOutput("map", height = "750px")
    ),
    
    # Enhanced control panel with clear step-by-step flow
    absolutePanel(
      top = 10,
      right = 10,
      width = 420,
      style = "z-index: 1000; max-height: 750px; overflow-y: auto;",
      # Match map height
      class = "control-panel",
      
      h4(icon("fire"), " Wildfire Analysis Workflow"),
      
      # Display Options (always visible)
      div(style = "margin-bottom: 15px;", h6("Display Options"), fluidRow(
        column(
          6,
          checkboxInput("show_buses", "Show Buses", TRUE),
          checkboxInput("show_lines", "Show Lines", TRUE)
        ),
        column(
          6,
          checkboxInput("show_fires", "Show Fires", TRUE),
          checkboxInput("show_fire_centers", "Fire Centers", FALSE)
        )
      )),
      
      hr(),
      
      # STEP 1: State Selection (always visible)
      div(
        class = "selection-step",
        div(
          class = "step-header",
          div(class = "step-number", "1"),
          h5("Select State", style = "margin: 0;")
        ),
        selectInput(
          "state_select",
          label = NULL,
          choices = c(
            "Choose a western state..." = "",
            setNames(cfg$western_states, tools::toTitleCase(cfg$western_states))
          ),
          selected = ""
        ),
        uiOutput("state_summary_dynamic")
      ),
      
      # STEP 2: Fire Intensity Selection (appears after state selection)
      conditionalPanel(
        condition = "input.state_select != ''",
        div(
          class = "selection-step active",
          div(
            class = "step-header",
            div(class = "step-number active", "2"),
            h5("Select Fire Intensity", style = "margin: 0;")
          ),
          p("Choose the fire intensity level to analyze:", style = "margin: 5px 0;"),
          uiOutput("intensity_dropdown_dynamic"),
          uiOutput("intensity_summary_dynamic")
        )
      ),
      
      # STEP 3: Additional Filters (appears after intensity selection)
      conditionalPanel(
        condition = "input.state_select != '' && input.fire_intensity_select != ''",
        div(
          class = "selection-step active",
          div(
            class = "step-header",
            div(class = "step-number active", "3"),
            h5("Additional Filters (Optional)", style = "margin: 0;")
          ),
          p("Refine your selection with additional criteria:", style = "margin: 5px 0; color: #666;"),
          
          div(
            class = "filter-group",
            h6(icon("leaf"), " Fuel Characteristics"),
            fluidRow(column(6, uiOutput(
              "fuel_type_filter_dynamic"
            )), column(
              6, uiOutput("fuel_category_filter_dynamic")
            ))
          ),
          
          div(class = "filter-group", h6(icon("building"), " Land Ownership"), fluidRow(
            column(6, uiOutput("landowner_category_filter_dynamic")), column(6, uiOutput("landowner_type_filter_dynamic"))
          )),
          
          uiOutput("filters_summary_dynamic"),
          
          conditionalPanel(
            condition = "output.has_active_filters",
            div(
              style = "text-align: center; margin-top: 10px;",
              actionButton(
                "clear_all_filters",
                "Clear All Filters",
                class = "btn-sm btn-outline-secondary",
                style = "width: 100%;"
              )
            )
          )
        )
      ),
      
      # STEP 4: Fire Event Selection (appears after intensity, shows filtered results)
      conditionalPanel(
        condition = "input.state_select != '' && input.fire_intensity_select != ''",
        div(
          class = "selection-step active",
          div(
            class = "step-header",
            div(class = "step-number active", "4"),
            h5("Select Fire Events (Multiple Allowed)", style = "margin: 0;")
          ),
          
          # description text
          p(
            "Select one or more fires for compound event analysis. Multiple fires will show combined cascading effects:",
            style = "margin: 5px 0; color: #555;"
          ),
          
          uiOutput("fire_event_dropdown_dynamic"),
          uiOutput("selected_fire_summary_dynamic")
        )
      ),
      
      # STEP 5: Analysis & Visualization (appears after fire selection)
      conditionalPanel(
        condition = "input.proceed_to_analysis > 0 && ((input.fire_events != null && input.fire_events.length > 0) || (input.fire_event != null && input.fire_event != ''))",
        div(
          class = "selection-step completed",
          div(
            class = "step-header",
            div(class = "step-number completed", "5"),
            h5("Analysis & Visualization", style = "margin: 0;")
          ),
          
          # Fire Visualization Controls
          div(
            class = "filter-group",
            h6(icon("palette"), " Fire Display Options"),
            radioButtons(
              "fire_color_mode",
              "Color Fires By:",
              choices = c(
                "Fire Intensity" = "intensity",
                "Fuel Category" = "attr_PrimaryFuelModel",
                "Landowner Category" = "attr_POOLandownerCategory"
              ),
              selected = "intensity",
              inline = FALSE
            ),
            
            fluidRow(column(
              6, checkboxInput("show_impact_zones", "Impact Zones", FALSE)
            ), column(
              6, checkboxInput("show_cascade_flow", "Cascade Flow", FALSE)
            ))
          ),
          
          # Enhanced Analysis Parameters Section
          div(
            class = "filter-group",
            h6(icon("cogs"), " Analysis Parameters"),
            
            # Step visualization controls
            sliderInput(
              "step",
              "Simulation Step",
              min = 1,
              max = 1,
              value = 1,
              animate = animationOptions(interval = 2000)
            ),
            
            # Two-column parameter layout with explanations
            fluidRow(column(
              6,
              div(
                style = "background: #e3f2fd; padding: 8px; border-radius: 4px; margin-bottom: 10px;",
                h6(style = "margin: 0; color: #1976d2;", icon("fire"), " Cascade Simulation"),
                numericInput(
                  "buffer_km",
                  "Impact Buffer (km):",
                  value = 2,
                  min = 0.5,
                  max = 10,
                  step = 0.5
                ),
                p(
                  "Determines direct fire impact range during cascade simulation.",
                  style = "font-size: 10px; margin: 0; color: #666;"
                )
              )
            ), column(
              6,
              div(
                style = "background: #f3e5f5; padding: 8px; border-radius: 4px; margin-bottom: 10px;",
                h6(style = "margin: 0; color: #7b1fa2;", icon("search-plus"), " TDA Analysis"),
                numericInput(
                  "proximity_km",
                  "Analysis Radius (km):",
                  value = 30,
                  min = 1,
                  max = 100,
                  step = 1
                ),
                p(
                  "Defines scope of topological analysis around fire center.",
                  style = "font-size: 10px; margin: 0; color: #666;"
                )
              )
            )),
            
            # Parameter relationship explanation
            uiOutput("parameter_explanation_dynamic"),
            
            # Analysis execution buttons
            fluidRow(column(
              6,
              actionButton(
                "run_cascade",
                "Run Cascade Simulation",
                class = "btn-info",
                style = "width: 100%; margin-bottom: 5px;"
              ),
              p("Simulates fire impact & grid failures", style = "font-size: 10px; text-align: center; margin: 0; color: #666;")
            ), column(
              6,
              actionButton(
                "run_wildfire_tda",
                "Run TDA Analysis",
                class = "btn-success",
                style = "width: 100%; margin-bottom: 5px;"
              ),
              p("Topological Data Analysis and Cascade Analysis", style = "font-size: 10px; text-align: center; margin: 0; color: #666;")
            ))
          ),
          
          # Results Display Section
          conditionalPanel(condition = "input.run_cascade > 0", div(
            class = "filter-group",
            h6(icon("chart-line"), " Cascade Results"),
            uiOutput("cascade_summary_dynamic"),
            
            # Add cascade status indicator
            div(
              id = "cascade-status",
              conditionalPanel(
                condition = "output.cascade_available",
                div(
                  class = "alert alert-success",
                  style = "padding: 8px; margin: 5px 0; font-size: 12px;",
                  icon("check-circle"),
                  " Cascade results ready for TDA analysis"
                )
              )
            )
          )),
          
          conditionalPanel(
            condition = "output.tda_results_available",
            div(
              class = "filter-group",
              h6(icon("project-diagram"), " TDA Results"),
              uiOutput("tda_results_dynamic"),
              
              # TDA results summary
              conditionalPanel(
                condition = "output.tda_plots_available",
                div(
                  style = "margin-top: 10px; text-align: center;",
                  actionButton(
                    "view_cascade_progression",
                    "View Cascade Progression",
                    class = "btn-sm btn-info",
                    style = "margin: 2px;"
                  ),
                  actionButton(
                    "view_persistence_diagrams",
                    "View Persistence Diagrams",
                    class = "btn-sm btn-info",
                    style = "margin: 2px;"
                  ),
                  br(),
                  downloadButton(
                    "download_tda_plots",
                    "Download All TDA Plots",
                    class = "btn-sm btn-success",
                    style = "margin: 5px;"
                  )
                )
              )
            )
          ),
          
          # Quick Export Section
          div(class = "filter-group", h6(icon("download"), " Quick Export"), fluidRow(
            column(
              6,
              downloadButton(
                "download_results",
                "Complete Analysis",
                class = "btn-secondary",
                style = "width: 100%; font-size: 11px;"
              )
            ), column(
              6,
              downloadButton(
                "download_plots_only",
                "Plots Only",
                class = "btn-secondary",
                style = "width: 100%; font-size: 11px;"
              )
            )
          ))
        )
      )
    ),
    
    div(
      class = "results-section",
      h2("Analysis Results Dashboard", style = "margin-bottom: 30px;"),
      
      # First row of plots
      fluidRow(style = "margin-bottom: 30px;", column(
        4, div(
          class = "plot-container",
          h4("Grid Resilience & TDA"),
          plotOutput("resilience_plot", height = "300px"),
          p(
            "Shows grid functionality, connectivity, and topological changes over time.",
            style = "font-size: 11px; color: #666; margin-top: 5px;"
          )
        )
      ), column(
        4, div(
          class = "plot-container",
          h4("Cascade Progression"),
          plotOutput("cascade_progression_plot", height = "300px"),
          p("Green line shows Wasserstein distance vs. nodes removed.", style = "font-size: 11px; color: #666; margin-top: 5px;")
        )
      ), column(
        4, div(
          class = "plot-container",
          h4("TDA Persistence Diagram"),
          plotOutput("tda_plot", height = "300px"),
          p("Topological features before/after wildfire impact.", style = "font-size: 11px; color: #666; margin-top: 5px;")
        )
      )),

      
      # Second row of plots
      fluidRow(style = "margin-bottom: 30px;", column(
        6, div(
          class = "plot-container",
          h4("TDA Before/After Comparison"),
          plotOutput("tda_comparison_plot", height = "300px"),
          p(
            "Direct comparison of persistence diagrams showing topological changes.",
            style = "font-size: 11px; color: #666; margin-top: 5px;"
          )
        )
      ), column(
        6, div(
          class = "plot-container",
          h4("Fire Impact Timeline"),
          plotOutput("fire_timeline_plot", height = "300px"),
          p("Fire progression over simulation steps.", style = "font-size: 11px; color: #666; margin-top: 5px;")
        )
      )),
      
      # Final results section
      fluidRow(
        column(
          12,
          div(
            class = "plot-container",
            h3("Comprehensive Analysis Results"),
            
            # Tabbed results section
            tabsetPanel(
              tabPanel(
                "TDA Summary",
                br(),
                conditionalPanel(
                  condition = "output.tda_results_available",
                  div(
                    class = "summary-box",
                    h5(icon("chart-line"), " Topological Data Analysis Results"),
                    verbatimTextOutput("tda_summary_text"),
                    br(),
                    conditionalPanel(
                      condition = "output.tda_plots_available",
                      div(
                        style = "text-align: center;",
                        actionButton("view_all_plots", "View All TDA Plots", class = "btn-info"),
                        br(),
                        br(),
                        downloadButton("download_tda_plots", "Download TDA Plots", class = "btn-success")
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "!output.tda_results_available",
                  div(
                    class = "alert alert-info",
                    icon("info-circle"),
                    " Run TDA analysis to see detailed results here."
                  )
                )
              ),
              
              tabPanel("System Status", br(), verbatimTextOutput("system_status")),
              
              tabPanel(
                "Export & Download",
                br(),
                h5("Download Options"),
                div(class = "filter-group", fluidRow(
                  column(
                    6,
                    downloadButton(
                      "download_results",
                      "Complete Analysis",
                      class = "btn-success",
                      style = "width: 100%; margin-bottom: 10px;"
                    ),
                    downloadButton(
                      "download_plots_only",
                      "TDA Plots Only",
                      class = "btn-info",
                      style = "width: 100%; margin-bottom: 10px;"
                    )
                  ),
                  column(
                    6,
                    downloadButton("download_gis", "GIS Data", class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
                    downloadButton(
                      "download_matrices",
                      "TDA Matrices",
                      class = "btn-secondary",
                      style = "width: 100%; margin-bottom: 10px;"
                    )
                  )
                ))
              )
            )
          )
        )
      )
    )
  )
)
# ====================================================================
# LAUNCH APPLICATION
# ====================================================================

shinyApp(ui = ui, server = server)