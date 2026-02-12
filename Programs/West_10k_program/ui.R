# =================================================================================================
# ui.R
# 
# User Interface Definition
#
# This file defines the complete UI layout for the Wildfire Grid Resilience Explorer
# including control panels, map display, and results dashboard
#
# Brandon Calvario
# =================================================================================================
library("shinyjs")
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
              6, checkboxInput("show_impact_zones", "Impact Buffer Zones", TRUE)
            ), column(
              6, checkboxInput("show_analysis_radius", "TDA Analysis Radius", TRUE)
            )),
            fluidRow(column(
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
              uiOutput("low_impact_indicator"),
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
              
              tabPanel(
                "Damage Assessment",
                br(),
                conditionalPanel(
                  condition = "output.cascade_available",
                  div(
                    # Bus type damage breakdown summary
                    h5(icon("bolt"), " Damage by Bus Type"),
                    uiOutput("damage_type_summary"),
                    br(),
                    
                    # Affected buses table
                    h5(icon("exclamation-triangle"), " Affected Buses (sorted by power impact)"),
                    p("Buses sorted from highest to lowest impact. Gen + Load buses cause the most",
                      " disruption when lost.", style = "font-size: 11px; color: #666;"),
                    DT::dataTableOutput("damage_bus_table"),
                    br(),
                    
                    # Affected branches table
                    h5(icon("project-diagram"), " Affected Branches"),
                    p("Transmission lines connected to failed buses, sorted by capacity (Rate A).",
                      style = "font-size: 11px; color: #666;"),
                    DT::dataTableOutput("damage_branch_table")
                  )
                ),
                conditionalPanel(
                  condition = "!output.cascade_available",
                  div(
                    class = "alert alert-info",
                    icon("info-circle"),
                    " Run cascade simulation to see damage assessment."
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