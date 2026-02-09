# =================================================================================================
# server.R
# 
# Server Function Definition
#
# This file defines the main server function that coordinates all reactive logic,
# event handlers, observers, and output rendering for the application
#
# Brandon Calvario
# =================================================================================================

source("global.R")
source("AttackAndCascade.R")
source("TopologicalDataWorkflowWF.R")

source("modules/initialization.R")
source("modules/reactive_handlers.R")
source("modules/map_management.R")
source("modules/ui_management.R")
source("modules/observers.R")
source("modules/ui_renderers.R")
source("modules/outputs_and_analysis.R")
source("modules/event_handlers.R")



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