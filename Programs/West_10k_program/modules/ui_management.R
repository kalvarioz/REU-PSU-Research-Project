# =================================================================================================
# modules/ui_management.R
# 
# UI State Management and Visibility Control
#
# This module contains functions for:
# - Managing UI visibility and transitions
# - Updating conditional panels
# - Resetting UI state
# - Validating UI readiness
#
# Brandon Calvario
# =================================================================================================

# ====================================================================
# UI STATE MANAGEMENT
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