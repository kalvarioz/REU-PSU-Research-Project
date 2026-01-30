# =================================================================================================
# modules/reactive_handlers.R
# 
# Reactive Value Management
#
# This module contains functions for:
# - Setting up cascade reactives
# - Managing fire data reactives
# - Handling filter reactives
# - Managing TDA reactives
# - Setting up map reactives
#
# Brandon Calvario
# =================================================================================================

# ====================================================================
# REACTIVE VALUE MANAGEMENT
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