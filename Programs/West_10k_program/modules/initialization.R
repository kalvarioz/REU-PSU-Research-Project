# =================================================================================================
# modules/initialization.R
# 
# System Initialization and Management Functions
#
# This module contains functions for:
# - Initializing server state and reactive values
# - Checking system readiness
# - Setting up error handlers
# - Processing initial wildfire data
#
# Brandon Calvario
# =================================================================================================

# ====================================================================
# INITIALIZATION & SYSTEM MANAGEMENT
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