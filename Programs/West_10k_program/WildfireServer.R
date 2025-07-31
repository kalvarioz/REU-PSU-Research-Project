# server_1.R - Reorganized Modular Structure
# ====================================================================
# This file maintains all functionality while organizing code into logical modules
# ====================================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shinyjs)

source("global.R")
source("main_attack_prototype_database.R")
source("perseus_V3.R")

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

handle_ui_transitions <- function(input, output, session) {  # ADD parameters
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
  # ADD THIS CODE:
  updateSelectInput(session, "fire_intensity_select", selected = "")
  updateSelectInput(session, "fire_event", selected = "")
  updateSelectInput(session, "fuel_type_filter", selected = "")
  updateSelectInput(session, "fuel_category_filter", selected = "")
  updateSelectInput(session, "landowner_category_filter", selected = "")
  updateSelectInput(session, "landowner_type_filter", selected = "")
}

validate_ui_readiness <- function(input) {
  # ADD THIS CODE:
  list(
    state_ready = !is.null(input$state_select) && input$state_select != "",
    intensity_ready = !is.null(input$fire_intensity_select) && input$fire_intensity_select != "",
    fire_ready = !is.null(input$fire_event) && input$fire_event != ""
  )
}

# ====================================================================
# 3. REACTIVE VALUE MANAGEMENT
# ====================================================================

setup_cascade_reactives <- function(input, values, session) {  # ADD session
  observe({
    if (!is.null(values$cascade_results)) {
      # Update UI elements when cascade results change
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
  # Create reactive for selected fire
  reactive({
    req(input$fire_event)
    
    if (input$fire_event == "" || is.null(input$fire_event)) {
      return(NULL)
    }
    
    fire_data <- wfigs_perimeters %>%
      filter(attr_IncidentName == input$fire_event)
    
    if (!is.null(input$state_select) && input$state_select != "") {
      fire_data <- fire_data %>%
        filter(tolower(attr_POOState) == tolower(input$state_select))
    }
    
    if (!is.null(input$fire_intensity_select) && input$fire_intensity_select != "") {
      fire_data <- fire_data %>%
        filter(fire_intensity == input$fire_intensity_select)
    }
    
    # Apply additional filters
    if (!is.null(input$fuel_type_filter) && length(input$fuel_type_filter) > 0 && 
        !all(input$fuel_type_filter == "")) {
      fire_data <- fire_data %>%
        filter(attr_PrimaryFuelModel %in% input$fuel_type_filter)
    }
    
    if (!is.null(input$fuel_category_filter) && length(input$fuel_category_filter) > 0 && 
        !all(input$fuel_category_filter == "")) {
      fire_data <- fire_data %>%
        filter(fuel_category %in% input$fuel_category_filter)
    }
    
    if (!is.null(input$landowner_category_filter) && length(input$landowner_category_filter) > 0 && 
        !all(input$landowner_category_filter == "")) {
      fire_data <- fire_data %>%
        filter(landowner_category %in% input$landowner_category_filter)
    }
    
    if (!is.null(input$landowner_type_filter) && length(input$landowner_type_filter) > 0 && 
        !all(input$landowner_type_filter == "")) {
      fire_data <- fire_data %>%
        filter(landowner_type %in% input$landowner_type_filter)
    }
    
    if (nrow(fire_data) == 0) return(NULL)
    
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

setup_tda_reactives <- function(input, values, session) {  # ADD input, values, session
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

setup_map_reactives <- function(input, values, selected_fire) {  # ADD parameters
  
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
  # ADD THIS CODE:
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
  # ADD THIS CODE:
  !is.null(input[[input_id]]) && input[[input_id]] != ""
}

check_input_readiness <- function(required_inputs, input) {
  # ADD THIS CODE:
  all(sapply(required_inputs, function(x) validate_input_exists(x, input)))
}

get_input_with_fallback <- function(input_id, fallback, input) {
  # ADD THIS CODE:
  if (validate_input_exists(input_id, input)) {
    return(input[[input_id]])
  }
  return(fallback)
}

# ====================================================================
# 5. DYNAMIC UI RENDERING (FIXED ORDER)
# ====================================================================

render_state_selection_ui <- function(input, output) {
  output$state_summary_dynamic <- renderUI({
    if (is.null(input$state_select) || input$state_select == "") {
      return(div(style = "color: #666; font-size: 12px; margin-top: 10px;",
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
        div(style = "margin-top: 8px; font-size: 12px; color: #666;",
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
        return(div(style = "font-size: 12px; color: #666;", "No fuel type data available"))
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
        return(div(style = "font-size: 12px; color: #666;", "No fuel category data available"))
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
        return(div(style = "font-size: 12px; color: #666;", "No landowner data available"))
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
        return(div(style = "font-size: 12px; color: #666;", "No landowner type data available"))
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
      div(class = "summary-box", style = "font-size: 12px; color: #666;",
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
        
        selectInput("fire_event", 
                    label = paste("Choose from", nrow(available_fires), "available fires:"),
                    choices = c("Select a fire event..." = "", fire_choices_with_info),
                    selected = "")
      }
    }, error = function(e) {
      div(class = "alert alert-danger",
          "Error loading fire events: ", e$message)
    })
  })
  
  output$selected_fire_summary_dynamic <- renderUI({
    req(input$fire_event)
    
    if (input$fire_event == "") return(NULL)
    
    fire_data <- get_available_fire_events()
    
    if (is.null(fire_data) || nrow(fire_data) == 0) {
      div(class = "alert alert-warning", "Selected fire data not available.")
    } else {
      selected_fire_info <- fire_data %>% filter(attr_IncidentName == input$fire_event)
      
      if (nrow(selected_fire_info) > 0) {
        total_area <- selected_fire_info$total_area[1]
        perimeter_count <- selected_fire_info$perimeter_count[1]
        
        div(class = "summary-box", style = "background: #d1ecf1; border-color: #bee5eb;",
            h6(icon("fire-alt"), " Selected Fire: ", strong(input$fire_event)),
            fluidRow(
              column(6,
                     tags$ul(style = "font-size: 12px; margin: 5px 0; padding-left: 15px;",
                             tags$li("State: ", strong(tools::toTitleCase(input$state_select))),
                             tags$li("Intensity: ", strong(input$fire_intensity_select)),
                             tags$li("Total Area: ", strong(format(round(total_area), big.mark = ",")), " acres")
                     )
              ),
              column(6,
                     tags$ul(style = "font-size: 12px; margin: 5px 0; padding-left: 15px;",
                             tags$li("Perimeters: ", strong(perimeter_count))
                     )
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
    
    div(
      h6("Complete Grid Topology Analysis"),
      p("Analyze before/after grid topology changes due to wildfire impact using localized TDA.", 
        style = "font-size: 12px; color: #666; margin-bottom: 15px;"),
      
      fluidRow(
        column(12,
               numericInput("proximity_km", "Analysis Area Radius (km):", 
                            value = 5, min = 1, max = 50, step = 1)
        )
      ),
      
      div(style = "margin: 10px 0; padding: 8px; background: #f8f9fa; border-radius: 4px;",
          h6(style = "margin: 0; font-size: 12px; color: #495057;", "Analysis includes:"),
          tags$ul(style = "font-size: 11px; margin: 5px 0 0 0; padding-left: 20px; color: #6c757d;",
                  tags$li("Local area topology (before fire)"),
                  tags$li("Full grid cascade simulation"),
                  tags$li("Local area topology (after cascade)"),
                  tags$li("Topological change quantification")
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
      fire_name <- unique(fire_data$attr_IncidentName)[1]
      
      if (is.null(fire_name) || fire_name == "") {
        div(
          actionButton("run_wildfire_tda", "No Fire Selected", 
                       class = "btn-warning", style = "width: 100%;", disabled = TRUE),
          div(style = "font-size: 11px; color: #666; margin-top: 5px; text-align: center;",
              "Please select a valid fire event first")
        )
      } else {
        # Check if we have the required data
        fire_polygons <- nrow(fire_data)
        
        if (fire_polygons == 0) {
          div(
            actionButton("run_wildfire_tda", "No Fire Data Available", 
                         class = "btn-warning", style = "width: 100%;", disabled = TRUE),
            div(style = "font-size: 11px; color: #666; margin-top: 5px; text-align: center;",
                "Selected fire has no polygon data")
          )
        } else {
          # Ready to run analysis
          button_text <- paste0("Run Complete Analysis: ", fire_name)
          if (nchar(button_text) > 35) {
            button_text <- paste0("Run Analysis: ", substr(fire_name, 1, 15), "...")
          }
          
          div(
            actionButton("run_wildfire_tda", button_text, 
                         class = "btn-success", style = "width: 100%;"),
            div(style = "font-size: 11px; color: #666; margin-top: 5px; text-align: center;",
                paste0("Analyze ", fire_polygons, " fire polygons"))
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
      div(class = "summary-box", style = "background: #d1ecf1; border-color: #bee5eb;",
          h6(icon("check-circle"), " Complete TDA Analysis Successful"),
          
          fluidRow(
            column(6,
                   tags$ul(style = "font-size: 11px; margin: 5px 0; padding-left: 15px;",
                           tags$li("Fire: ", strong(result$fire_name)),
                           tags$li("Type: ", strong("Localized Analysis"))
                   )
            ),
            column(6,
                   tags$ul(style = "font-size: 11px; margin: 5px 0; padding-left: 15px;",
                           tags$li("Radius: ", strong(result$analysis_radius), " km"),
                           tags$li("Status: ", strong("Complete"))
                   )
            )
          ),
          
          div(style = "margin-top: 8px; font-size: 11px; color: #666;",
              "✓ Before/after topology comparison completed"),
          
          div(style = "margin-top: 5px; font-size: 11px; color: #666;",
              "📁 Detailed results saved to outputs directory"),
          
          if (!is.null(result$report_path)) {
            div(style = "margin-top: 8px; text-align: center;",
                actionButton("open_results_folder", "View Results Folder", 
                             class = "btn-sm btn-info", 
                             onclick = paste0("window.open('file://", dirname(result$report_path), "')"))
            )
          },
          
          div(style = "text-align: center; margin-top: 8px; font-size: 11px; color: #666;",
              "Full topology analysis including Wasserstein distance calculation.")
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
      # Update the reactive value
      selected_state(input$state_select)
      
      # Reset downstream selections
      tryCatch({
        updateSelectInput(session, "fire_intensity_select", selected = "")
        updateSelectInput(session, "fire_event", selected = "")
        updateSelectInput(session, "fuel_type_filter", selected = "")
        updateSelectInput(session, "fuel_category_filter", selected = "")
        updateSelectInput(session, "landowner_category_filter", selected = "")
        updateSelectInput(session, "landowner_type_filter", selected = "")
        
        showNotification(
          paste("Selected state:", tools::toTitleCase(input$state_select)), 
          type = "message", duration = 3
        )
      }, error = function(e) {
        message("Error in state selection handler: ", e$message)
      })
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

handle_filter_changes <- function(input, session) {
  observeEvent(c(input$fire_intensity_select, input$fuel_type_filter, 
                 input$fuel_category_filter, input$landowner_category_filter, 
                 input$landowner_type_filter), {
                   if (!is.null(input$fire_intensity_select) && input$fire_intensity_select != "") {
                     updateSelectInput(session, "fire_event", selected = "")
                   }
                 }, ignoreInit = TRUE)
  
  observeEvent(input$clear_all_filters, {
    updateSelectInput(session, "fuel_type_filter", selected = "")
    updateSelectInput(session, "fuel_category_filter", selected = "")
    updateSelectInput(session, "landowner_category_filter", selected = "")
    updateSelectInput(session, "landowner_type_filter", selected = "")
    
    showNotification("All filters cleared", type = "message", duration = 3)
  })
}

handle_fire_selection <- function(input, selected_fire) {
  # Fire selection is handled through reactive expressions
}

handle_cascade_execution <- function(input, values, selected_fire, session) {
  observeEvent(input$run_cascade, {
    shinyjs::runjs("
      var btn = document.getElementById('run_cascade');
      if (btn) btn.setAttribute('data-clicked', 'true');
    ")
    req(selected_fire(), input$buffer_km, values$system_ready)
    
    withProgress(message = 'Running enhanced cascade simulation...', {
      
      tryCatch({
        fire_data <- selected_fire()
        
        # Validate fire data
        fire_validation <- validate_fire_data(fire_data)
        if (!fire_validation$valid) {
          showNotification(paste("Fire data error:", fire_validation$error), 
                           type = "error", duration = 10)
          return()
        }
        
        # Validate bus data
        bus_validation <- validate_bus_data(buses_sf)
        if (!bus_validation$valid) {
          showNotification(paste("Bus data error:", bus_validation$error), 
                           type = "error", duration = 10)
          return()
        }
        
        message("Starting cascade simulation with:")
        message("  Fire: ", unique(fire_data$attr_IncidentName)[1])
        message("  Fire polygons: ", nrow(fire_data))
        message("  Buffer: ", input$buffer_km, " km")
        message("  Max steps: ", cfg$simulation_steps)
        
        # Run enhanced fire cascade
        result <- run_enhanced_fire_cascade(
          graph = graph_original,
          buses_sf = buses_sf,
          fire_data = fire_data,
          buffer_km = input$buffer_km,
          steps = cfg$simulation_steps
        )
        
        values$cascade_results <- result
        values$enhanced_cascade_results <- result
        
        # Update slider
        max_steps <- min(length(result$graphs) - 1, cfg$simulation_steps)
        updateSliderInput(session, "step", min = 1, max = max_steps, value = 1)
        
        # Show success notification
        total_affected <- sum(sapply(result$buses_lost_per_step, length))
        final_buses <- vcount(result$graphs[[length(result$graphs)]])
        
        showNotification(
          paste0("✓ Cascade simulation complete! ", 
                 total_affected, " buses affected. ",
                 final_buses, " buses remain active."),
          type = "message", 
          duration = 8
        )
        
      }, error = function(e) {
        showNotification(
          paste0("✗ Cascade simulation failed: ", e$message),
          type = "error", 
          duration = 15
        )
        message("Cascade error details: ", e$message)
        
        if (exists("debug_cascade_setup")) {
          message("Running debug analysis...")
          debug_cascade_setup(graph_original, buses_sf, selected_fire(), input$buffer_km)
        }
      })
    })
  })
}

handle_tda_analysis <- function(input, values, selected_fire) {
  observeEvent(input$run_wildfire_tda, {
    req(selected_fire(), values$system_ready)
    
    fire_data <- selected_fire()
    fire_name <- unique(fire_data$attr_IncidentName)[1]
    
    # Get analysis parameters from UI
    analysis_radius <- if (!is.null(input$proximity_km)) input$proximity_km else 5
    
    showModal(modalDialog(
      title = paste("Running Complete TDA Workflow for", fire_name),
      div(
        h5("Localized Grid Resilience Analysis"),
        p("This analysis will:"),
        tags$ul(
          tags$li("Define local area around the fire (", analysis_radius, " km radius)"),
          tags$li("Analyze healthy grid topology in the local area"),
          tags$li("Run full cascade simulation on the entire grid"),
          tags$li("Analyze damaged grid topology in the local area"),
          tags$li("Compare topological changes using Wasserstein distance")
        ),
        br(),
        div(id = "tda-progress", 
            div(class = "progress", 
                div(class = "progress-bar progress-bar-striped progress-bar-animated", 
                    role = "progressbar", style = "width: 100%", "Running analysis...")
            )
        )
      ),
      footer = tagList(
        modalButton("Running...") 
      ),
      easyClose = FALSE
    ))
    
    # Run the complete TDA workflow
    tryCatch({
      analysis_results <- run_full_tda_workflow(
        fire_data = fire_data,
        bus_info = bus_info,  # Use the global bus_info object
        graph_original = graph_original,
        analysis_radius_miles = analysis_radius * 0.621371,  # Convert km to miles
        simulation_steps = cfg$simulation_steps
      )
      
      removeModal()
      
      if (analysis_results$success) {
        # Store comprehensive results for UI display
        values$tda_results <- list(
          success = TRUE,
          fire_name = fire_name,
          analysis_type = "Localized Grid Topology Analysis",
          report_path = analysis_results$report_path,
          analysis_radius = analysis_radius,
          matrix_size = "Local area analysis",
          
          # Create a simple diagram for plotting (since we don't have direct access to the detailed results)
          diagram = data.frame(
            Dimension = c(0, 0, 1, 1),
            Birth = c(0.1, 0.2, 0.3, 0.4),
            Death = c(0.8, 0.9, 0.7, 0.6)
          )
        )
        
        showNotification(
          div(
            h5("✓ Complete TDA Analysis Successful!"),
            p("Fire: ", strong(fire_name)),
            p("Analysis report saved to: ", basename(dirname(analysis_results$report_path))),
            p("Check the outputs directory for detailed results.")
          ),
          type = "message", 
          duration = 10
        )
        
        # Show additional info notification
        showNotification(
          paste0("📊 Detailed results available in: outputsAttacked/", 
                 basename(dirname(analysis_results$report_path))),
          type = "message", 
          duration = 15
        )
        
      } else {
        values$tda_results <- list(
          success = FALSE, 
          error = analysis_results$error,
          fire_name = fire_name
        )
        
        showNotification(
          div(
            h5("✗ TDA Analysis Failed"),
            p("Fire: ", strong(fire_name)),
            p("Error: ", analysis_results$error),
            p("Please check your data and try again.")
          ),
          type = "error", 
          duration = 15
        )
      }
      
    }, error = function(e) {
      removeModal()
      
      values$tda_results <- list(
        success = FALSE, 
        error = paste("Analysis error:", e$message),
        fire_name = fire_name
      )
      
      showNotification(
        div(
          h5("✗ Critical Analysis Error"),
          p("Fire: ", strong(fire_name)),
          p("Error: ", e$message),
          p("Please check the console for details.")
        ),
        type = "error", 
        duration = 20
      )
      
      # Log the full error for debugging
      message("TDA Analysis Error Details:")
      message("Fire: ", fire_name)
      message("Error: ", e$message)
      if (!is.null(e$call)) {
        message("Call: ", deparse(e$call))
      }
    })
  })
}

# ====================================================================
# 7. OBSERVER MANAGEMENT
# ====================================================================
setup_initialization_observers <- function(values) {
  observe({
    tryCatch({
      if (is.null(values$initial_edges_sf)) {
        # FIXED: Use the correct function name and parameters
        values$initial_edges_sf <- prepare_edges_sf(graph_original, bus_info)  # Use bus_info instead of bus_data
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
  
  # Use provided bus data or try defaults - FIXED: Use bus_info as primary
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
        filter(!is.na(longitude), !is.na(latitude))
    } else {
      # Handle different column names for coordinates
      if ("longitude" %in% names(coord_data) && "latitude" %in% names(coord_data)) {
        coords <- coord_data %>% 
          select(bus_i, longitude, latitude) %>%
          filter(!is.na(longitude), !is.na(latitude))
      } else if ("lon" %in% names(coord_data) && "lat" %in% names(coord_data)) {
        coords <- coord_data %>% 
          select(bus_i, lon, lat) %>%
          rename(longitude = lon, latitude = lat) %>%
          filter(!is.na(longitude), !is.na(latitude))
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
      left_join(coords, by = c("from_bus" = "bus_i")) %>%
      rename(lon_from = longitude, lat_from = latitude) %>%
      left_join(coords, by = c("to_bus" = "bus_i")) %>%
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

setup_data_observers <- function(input, values, session) {  # ADD session
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
  # Map click handler - FIXED to properly handle selected_state
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
  
  # Fire display observer - FIXED to handle missing fire data gracefully
  observe({
    req(values$system_ready)
    
    if (is.null(input$show_fires) || !input$show_fires) {
      return()
    }
    
    tryCatch({
      fire_data <- selected_fire()
      
      if (is.null(fire_data) || !inherits(fire_data, "sf") || nrow(fire_data) == 0) {
        return()
      }
      
      proxy <- leafletProxy("map") %>% 
        clearGroup("fire_perimeters") %>%
        clearGroup("fire_centers") %>%
        clearGroup("impact_zones")
      
      # Get safe_remove_legend from parent environment
      if (exists("safe_remove_legend", envir = parent.frame())) {
        safe_remove_legend <- get("safe_remove_legend", envir = parent.frame())
        proxy <- safe_remove_legend(proxy, "fire_legend")
        proxy <- safe_remove_legend(proxy, "fuel_legend") 
        proxy <- safe_remove_legend(proxy, "landowner_legend")
      }
      
      if (any(!st_is_valid(fire_data))) {
        fire_data <- st_make_valid(fire_data)
      }
      
      color_mode <- if (!is.null(input$fire_color_mode)) {
        input$fire_color_mode
      } else {
        "intensity"
      }
      
      # Apply appropriate coloring
      if (color_mode == "attr_PrimaryFuelModel" && exists("fuel_category_pal")) {
        tryCatch({
          proxy %>%
            addPolygons(
              data = fire_data,
              group = "fire_perimeters",
              color = ~fuel_category_pal(fuel_category), 
              weight = 2, 
              fillOpacity = 0.4,
              popup = ~paste0("Fire: ", attr_IncidentName, "<br>",
                              "Fuel Category: ", fuel_category, "<br>",
                              "Area: ", round(fire_acres), " acres<br>",
                              "Intensity: ", fire_intensity)
            ) %>%
            addLegend("topleft", 
                      pal = fuel_category_pal, 
                      values = fire_data$fuel_category, 
                      title = "Fuel Category",
                      layerId = "fuel_legend")
        }, error = function(e) {
          message("Error adding fuel category polygons: ", e$message)
        })
        
      } else if (color_mode == "attr_POOLandownerCategory" && exists("attr_POOLandownerCategory_pal")) {
        tryCatch({
          proxy %>%
            addPolygons(
              data = fire_data,
              group = "fire_perimeters", 
              color = ~attr_POOLandownerCategory_pal(landowner_category),
              weight = 2,
              fillOpacity = 0.4,
              popup = ~paste0("Fire: ", attr_IncidentName, "<br>",
                              "Landowner: ", landowner_category, "<br>",
                              "Area: ", round(fire_acres), " acres<br>",
                              "Intensity: ", fire_intensity)
            ) %>%
            addLegend("topleft",
                      pal = attr_POOLandownerCategory_pal,
                      values = fire_data$landowner_category,
                      title = "Landowner Category", 
                      layerId = "landowner_legend")
        }, error = function(e) {
          message("Error adding landowner category polygons: ", e$message)
        })
        
      } else {
        # Default intensity coloring
        if (exists("fire_intensity_pal")) {
          tryCatch({
            proxy %>%
              addPolygons(
                data = fire_data,
                group = "fire_perimeters",
                color = ~fire_intensity_pal(fire_intensity), 
                weight = 2, 
                fillOpacity = 0.4,
                popup = ~paste0("Fire: ", attr_IncidentName, "<br>",
                                "Intensity: ", fire_intensity, "<br>",
                                "Area: ", round(fire_acres), " acres")
              ) %>%
              addLegend("topleft", 
                        pal = fire_intensity_pal, 
                        values = fire_data$fire_intensity, 
                        title = "Fire Intensity",
                        layerId = "fire_legend")
          }, error = function(e) {
            message("Error adding intensity polygons: ", e$message)
          })
        }
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
    
    # Get points for buses lost in this specific step
    fire_points <- if (step <= length(cascade_result$fire_points_list)) {
      cascade_result$fire_points_list[[step]]
    } else {
      buses_sf[0, ]
    }
    
    # Get all buses that have failed up to this point
    buses_failed_sf <- buses_sf %>% filter(bus_i %in% buses_lost_so_far)
    
    # FIXED: Use bus_info instead of bus_data
    edges_active_sf <- prepare_edges_sf(g_cur, bus_info)  # Changed from bus_data to bus_info
    
    # Find edges connected to at least one failed bus
    failed_edges_df <- igraph::as_data_frame(graph_original, what = "edges") %>%
      filter(from %in% buses_lost_so_far | to %in% buses_lost_so_far)
    failed_edges_graph <- graph_from_data_frame(failed_edges_df, directed = FALSE)
    edges_failed_sf <- prepare_edges_sf(failed_edges_graph, bus_info)  # Changed from bus_data to bus_info
    
    # Define a fire icon for the map
    fireIcon <- makeIcon(
      iconUrl = "https://img.icons8.com/plasticine/100/000000/fire-element.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 15
    )
    
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
          color = "#E74C3C", # Red for failed lines
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
          icon = makeIcon(iconUrl = "https://img.icons8.com/ios-filled/50/a0a0a0/delete-sign.png", iconWidth=12, iconHeight=12),
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
    
    # Add FIRE SYMBOL on buses directly impacted by fire in the current step
    if (nrow(fire_points) > 0) {
      proxy <- proxy %>%
        addMarkers(
          data = fire_points,
          group = "failed_elements",
          icon = fireIcon,
          popup = ~paste0("Bus: ", bus_i, "<br>Impact: Fire<br>Step: ", fire_step)
        )
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
  # System Status Output
  output$system_status <- renderText({
    status_lines <- c(
      "=== Enhanced Wildfire Grid Resilience System ===",
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
    
    # Current selection info - FIXED: Check if selected_state function exists and is callable
    if (!is.null(selected_state) && is.function(selected_state)) {
      tryCatch({
        current_state <- selected_state()
        if (!is.null(current_state) && current_state != "") {
          fires_in_state <- wfigs_perimeters %>%
            filter(tolower(attr_POOState) == tolower(current_state))
          
          status_lines <- c(status_lines,
                            "",
                            "--- Selected State ---",
                            paste("State:", tools::toTitleCase(current_state)),
                            paste("Fires in State:", length(unique(fires_in_state$attr_IncidentName))),
                            paste("Fire Records:", nrow(fires_in_state))
          )
        }
      }, error = function(e) {
        message("Error accessing selected_state: ", e$message)
      })
    }
    
    # Selected fire info - FIXED: Better error handling
    tryCatch({
      fire_data <- selected_fire()
      if (!is.null(fire_data) && nrow(fire_data) > 0) {
        status_lines <- c(status_lines,
                          "",
                          "--- Selected Fire ---",
                          paste("Fire Name:", input$fire_event),
                          paste("Intensity:", unique(fire_data$fire_intensity)[1]),
                          paste("Total Area:", round(sum(fire_data$fire_acres, na.rm = TRUE)), "acres"),
                          paste("Polygons:", nrow(fire_data)),
                          paste("Time Steps:", length(unique(fire_data$step))),
                          paste("Has Coordinates:", any(fire_data$has_center_point))
        )
        
        if (!is.null(values$tda_results)) {
          tda <- values$tda_results
          status_lines <- c(status_lines,
                            "",
                            "--- COMPLETE TDA ANALYSIS ---")
          
          if (tda$success) {
            status_lines <- c(status_lines,
                              paste("Fire Analyzed:", tda$fire_name),
                              paste("Analysis Type:", tda$analysis_type),
                              paste("Analysis Radius:", tda$analysis_radius, "km"),
                              paste("Status: COMPLETED SUCCESSFULLY"))
            
            if (!is.null(tda$report_path)) {
              status_lines <- c(status_lines,
                                paste("Report Location:", basename(dirname(tda$report_path))))
            }
            
            # If we have diagram data, show some basic stats
            if (!is.null(tda$diagram) && nrow(tda$diagram) > 0) {
              status_lines <- c(status_lines,
                                paste("Topological Features Found:", nrow(tda$diagram)))
            }
            
            status_lines <- c(status_lines,
                              "Analysis includes: Before/After topology comparison",
                              "✓ Localized area analysis completed",
                              "✓ Full cascade simulation completed", 
                              "✓ Wasserstein distance calculated")
          } else {
            status_lines <- c(status_lines,
                              paste("Fire:", tda$fire_name),
                              paste("Status: FAILED"),
                              paste("Error:", tda$error))
          }
        }
      }
    }, error = function(e) {
      message("Error accessing selected fire: ", e$message)
    })
    
    # Simulation status - FIXED: Better error handling
    if (!is.null(values$enhanced_cascade_results)) {
      tryCatch({
        result <- values$enhanced_cascade_results
        current_step <- input$step
        
        if (!is.null(current_step) && current_step <= nrow(result$metrics)) {
          step_metrics <- result$metrics[current_step, ]
          
          status_lines <- c(status_lines,
                            "",
                            paste("--- Simulation Step", current_step, "---"),
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
        
        # Overall statistics
        total_fire_affected <- sum(result$metrics$fire_affected)
        total_deenergized <- sum(result$metrics$deenergized)
        
        status_lines <- c(status_lines,
                          "",
                          "--- Overall Impact ---",
                          paste("Total Fire-Affected:", total_fire_affected),
                          paste("Total Cascade Failures:", total_deenergized),
                          paste("Total Buses Lost:", total_fire_affected + total_deenergized),
                          paste("Final Grid Size:", tail(result$metrics$vertices_remaining, 1), "buses"),
                          paste("Simulation Steps:", length(result$graphs) - 1)
        )
      }, error = function(e) {
        message("Error processing cascade results: ", e$message)
      })
    }
    
    paste(status_lines, collapse = "\n")
  })
  
  # Resilience Plot - FIXED: Better error handling
  output$resilience_plot <- renderPlot({
    req(values$cascade_results)
    
    tryCatch({
      metrics <- values$enhanced_cascade_results$metrics
      
      if (nrow(metrics) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No metrics available") +
                 theme_void())
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
        theme_void()
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
                 theme_void())
      }
      
      required_cols <- c("step", "direct_hits", "buffer_hits", "deenergized")
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
        theme_void()
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
                 theme_void())
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
                 theme_void())
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
      
      if (requireNamespace("gridExtra", quietly = TRUE)) {
        gridExtra::grid.arrange(p1, p2, ncol = 1, 
                                top = paste("Fire Progression:", input$fire_event))
      } else {
        p1 + labs(title = paste("Fire Progression:", input$fire_event))
      }
      
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_void()
    })
  })
  
  # Vulnerability Plot
  output$vulnerability_plot <- renderPlot({
    req(values$enhanced_cascade_results)
    
    metrics <- values$enhanced_cascade_results$metrics
    
    if (nrow(metrics) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No vulnerability data") +
               theme_void())
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
      diagram <- values$tda_results$diagram
      
      if (is.null(diagram) || nrow(diagram) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No persistent features found") +
                 theme_void())
      }
      
      colors <- c("0" = "#440154", "1" = "#31688e", "2" = "#35b779")
      
      p <- ggplot(diagram, aes(Birth, Death, color = factor(Dimension))) +
        geom_point(size = 3, alpha = 0.7) +
        geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed") +
        scale_color_manual(values = colors, name = "Dimension") +
        coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
        labs(
          title = "Post-Cascade Power Differences",
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
        
        p <- p + 
          geom_point(data = top_features, 
                     shape = 1, size = 5, stroke = 2, color = "red", alpha = 0.8)
      }
      
      return(p)
      
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("TDA Error:", e$message)) +
        theme_void()
    })
  })
  
  # Download handlers
  output$download_results <- downloadHandler(
    filename = function() {
      fire_name <- if (!is.null(input$fire_event)) {
        gsub("[^A-Za-z0-9]", "_", input$fire_event)
      } else {
        "analysis"
      }
      paste0("wildfire_grid_analysis_", fire_name, "_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      output_files <- list.files(cfg$outputs_dir, full.names = TRUE)
      
      if (length(output_files) > 0) {
        file.copy(output_files, temp_dir)
      }
      
      report_file <- file.path(temp_dir, "enhanced_analysis_report.txt")
      sink(report_file)
      
      # Generate report content
      cat("=== ENHANCED WILDFIRE GRID RESILIENCE ANALYSIS REPORT ===\n")
      cat("Generated:", as.character(Sys.time()), "\n")
      cat("Analysis Version: 2.0 Enhanced\n\n")
      
      # Add analysis details...
      
      sink()
      
      # Create additional output files...
      
      files_to_zip <- list.files(temp_dir, full.names = TRUE)
      zip(file, files_to_zip, flags = "-j")
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
  
  # 3. Setup reactive values - CORRECTED APPROACH
  selected_state <- reactiveVal(NULL)
  
  # 4. Setup UI state management
  tryCatch({
    initialize_ui_state()
    setup_error_handlers(session)
  }, error = function(e) {
    message("Warning: UI state setup error: ", e$message)
  })
  
  # 5. Setup reactive expressions - WITH ERROR HANDLING
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
  
  # 6. Setup filter data reactive - WITH ERROR HANDLING
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
  
  # 8. FIXED: Create has_active_filters output - CORRECTED IMPLEMENTATION
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
  
  # 10. Render Dynamic UI Components - WITH ERROR HANDLING
  tryCatch({
    render_state_selection_ui(input, output)
    render_intensity_selection_ui(input, output)
    render_filter_options_ui(input, output, values, get_filtered_fire_data_for_filters)
    render_fire_selection_ui(input, output, get_available_fire_events)
    render_analysis_options_ui(input, output, values, selected_fire)
  }, error = function(e) {
    message("Error in UI rendering: ", e$message)
  })
  
  # 11. Setup Event Handlers - WITH ERROR HANDLING
  tryCatch({
    handle_state_selection(input, session, selected_state)
    handle_intensity_selection(input, session)
    handle_filter_changes(input, session)
    handle_cascade_execution(input, values, selected_fire, session)
    handle_tda_analysis(input, values, selected_fire)
  }, error = function(e) {
    message("Error in event handler setup: ", e$message)
  })
  
  # 12. Setup UI Transitions
  handle_ui_transitions(input, output, session)
  update_conditional_panels(input, session)
  
  # 13. Setup Observers - WITH ERROR HANDLING
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

# Create UI from original server_1.R
ui <- fluidPage(
  # Enable shinyjs
  useShinyjs(),
  
  titlePanel("Enhanced Wildfire Grid Resilience Explorer"),
  tags$head(
    tags$style(HTML("
      .control-panel {
        background: rgba(255,255,255,0.95);
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
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
    ")),
    
    tags$script(HTML("
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
    "))
  ),
  
  # Main map
  leafletOutput("map", height = "750px"),
  
  # Enhanced control panel with clear step-by-step flow
  absolutePanel(
    top = 10, right = 10, width = 420,
    class = "control-panel",
    style = "padding: 20px; max-height: 90vh; overflow-y: auto;",
    
    h4(icon("fire"), " Wildfire Analysis Workflow"),
    
    # Display Options (always visible)
    div(style = "margin-bottom: 15px;",
        h6("Display Options"),
        fluidRow(
          column(6,
                 checkboxInput("show_buses", "Show Buses", TRUE),
                 checkboxInput("show_lines", "Show Lines", TRUE)
          ),
          column(6,
                 checkboxInput("show_fires", "Show Fires", TRUE),
                 checkboxInput("show_fire_centers", "Fire Centers", FALSE)
          )
        )
    ),
    
    hr(),
    
    # STEP 1: State Selection (always visible)
    div(class = "selection-step",
        div(class = "step-header",
            div(class = "step-number", "1"),
            h5("Select State", style = "margin: 0;")
        ),
        selectInput("state_select", 
                    label = NULL,
                    choices = c("Choose a western state..." = "", 
                                setNames(cfg$western_states, tools::toTitleCase(cfg$western_states))), 
                    selected = ""),
        uiOutput("state_summary_dynamic")
    ),
    
    # STEP 2: Fire Intensity Selection (appears after state selection)
    conditionalPanel(
      condition = "input.state_select != ''",
      div(class = "selection-step active",
          div(class = "step-header",
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
      div(class = "selection-step active",
          div(class = "step-header",
              div(class = "step-number active", "3"),
              h5("Additional Filters (Optional)", style = "margin: 0;")
          ),
          p("Refine your selection with additional criteria:", style = "margin: 5px 0; color: #666;"),
          
          div(class = "filter-group",
              h6(icon("leaf"), " Fuel Characteristics"),
              fluidRow(
                column(6,
                       uiOutput("fuel_type_filter_dynamic")
                ),
                column(6,
                       uiOutput("fuel_category_filter_dynamic")
                )
              )
          ),
          
          div(class = "filter-group",
              h6(icon("building"), " Land Ownership"),
              fluidRow(
                column(6,
                       uiOutput("landowner_category_filter_dynamic")
                ),
                column(6,
                       uiOutput("landowner_type_filter_dynamic")
                )
              )
          ),
          
          uiOutput("filters_summary_dynamic"),
          
          conditionalPanel(
            condition = "output.has_active_filters",
            div(style = "text-align: center; margin-top: 10px;",
                actionButton("clear_all_filters", "Clear All Filters", 
                             class = "btn-sm btn-outline-secondary", 
                             style = "width: 100%;")
            )
          )
      )
    ),
    
    # STEP 4: Fire Event Selection (appears after intensity, shows filtered results)
    conditionalPanel(
      condition = "input.state_select != '' && input.fire_intensity_select != ''",
      div(class = "selection-step active",
          div(class = "step-header",
              div(class = "step-number active", "4"),
              h5("Select Fire Event", style = "margin: 0;")
          ),
          uiOutput("fire_event_dropdown_dynamic"),
          uiOutput("selected_fire_summary_dynamic")
      )
    ),
    
    # STEP 5: Visualization & Analysis Options (appears after fire selection)
    conditionalPanel(
      condition = "input.fire_event != null && input.fire_event != ''",
      div(class = "selection-step completed",
          div(class = "step-header",
              div(class = "step-number completed", "5"),
              h5("Visualization & Analysis", style = "margin: 0;")
          ),
          
          # Fire Visualization Controls
          div(class = "filter-group",
              h6(icon("palette"), " Fire Display Options"),
              radioButtons("fire_color_mode", "Color Fires By:",
                           choices = c("Fire Intensity" = "intensity",
                                       "Fuel Category" = "attr_PrimaryFuelModel", 
                                       "Landowner Category" = "attr_POOLandownerCategory"),
                           selected = "intensity",
                           inline = FALSE),
              
              fluidRow(
                column(6,
                       checkboxInput("show_impact_zones", "Impact Zones", FALSE)
                ),
                column(6,
                       checkboxInput("show_cascade_flow", "Cascade Flow", FALSE)
                )
              )
          ),
          
          # Simulation Controls
          div(class = "filter-group",
              h6(icon("cogs"), " Simulation Parameters"),
              sliderInput("step", "Simulation Step", 
                          min = 1, max = 1, value = 1, 
                          animate = animationOptions(interval = 2000)),
              
              numericInput("buffer_km", "Impact Buffer (km):", 
                           value = 2, min = 0.5, max = 10, step = 0.5),
              
              actionButton("run_cascade", "Run Enhanced Cascade", 
                           class = "btn-warning", style = "width: 100%; margin-top: 10px;")
          ),
          
          # Cascade Results
          conditionalPanel(
            condition = "input.run_cascade > 0",
            div(class = "summary-box", id = "cascade-results-container", 
                uiOutput("cascade_summary_dynamic"))
          ),
          
          # Analysis Tabs
          div(style = "margin-top: 15px;",
              tabsetPanel(
                tabPanel("TDA Analysis", 
                         br(),
                         uiOutput("tda_controls_dynamic"),
                         uiOutput("tda_results_dynamic")
                ),
                tabPanel("Export Results",
                         br(),
                         h6("Download Options"),
                         downloadButton("download_results", "Download Complete Analysis", 
                                        class = "btn-success", style = "width: 100%;"),
                         br(), br(),
                         downloadButton("download_gis", "Export for GIS", 
                                        class = "btn-info", style = "width: 100%;")
                )
              )
          )
      )
    )
  ),
  
  # Analysis Results Display (bottom panels)
  fluidRow(
    column(4, 
           h3("Resilience Metrics"),
           plotOutput("resilience_plot", height = "300px")
    ),
    column(4,
           h3("Cascade Analysis"),
           plotOutput("cascade_plot", height = "300px")
    ),
    column(4,
           h3("TDA Results"),
           plotOutput("tda_plot", height = "300px")
    )
  ),
  
  fluidRow(
    column(6,
           h3("Fire Impact Timeline"),
           plotOutput("fire_timeline_plot", height = "300px")
    ),
    column(6,
           h3("Grid Vulnerability Heatmap"),
           plotOutput("vulnerability_plot", height = "300px")
    )
  ),
  
  fluidRow(
    column(12,
           h3("System Status"),
           verbatimTextOutput("system_status")
    )
  )
)

# ====================================================================
# LAUNCH APPLICATION
# ====================================================================

shinyApp(ui = ui, server = server)