# =================================================================================================
# app.R - Main Application Entry Point
# Modular Wildfire Grid Resilience Application
# Brandon Calvario
# =================================================================================================

# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shinyjs)
# Source global configuration and utilities
source("global.R")
source("AttackAndCascade.R")
source("TopologicalDataWorkflowWF.R")
# Source all module files
source("modules/initialization.R")
source("modules/ui_management.R")
source("modules/reactive_handlers.R")
source("modules/input_validation.R")
source("modules/ui_renderers.R")
source("modules/event_handlers.R")
source("modules/observers.R")
source("modules/map_management.R")
source("modules/data_processing.R")
source("modules/outputs_and_analysis.R")
# Source UI and server definitions
source("ui.R")
source("server.R")
# Launch the application
shinyApp(ui = ui, server = server)