# =================================================================================================
# app.R - Main Application Entry Point
# Modular Wildfire Grid Resilience Application
# Brandon Calvario
# =================================================================================================

# Set working directory
setwd("/home/calvario/Documents/github_projects/REU-PSU-Research-Project/Programs/West_10k_program")

# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shinyjs)

# Source global configuration, ALL dependencies, and run initialization.
# global.R sources AttackAndCascade.R, TopologicalDataWorkflowWF.R, and all
# modules/ files internally, then runs initialize_system(). This ensures
# everything works regardless of how the app is launched.
source("global.R")

# Source UI and server definitions
source("ui.R")
source("server.R")

# Launch the application
shinyApp(ui = ui, server = server)