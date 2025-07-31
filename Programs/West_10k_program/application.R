# app.R - Clean Shiny Application Launcher
# ====================================================================

# This file launches the Shiny application
# All example functions have been moved to examples.R

cat("=== Wildfire Grid Resilience Explorer ===\n")
if (!exists("system_initialized")) {
  source("perseus_V3.R")    # Load Perseus functions first
  source("global.R")        # Then global (which now uses Perseus functions)
  source("main_attack_prototype_database.R")
}

# Verify system and launch
if (!exists("system_initialized") || !system_initialized) {
  stop("System initialization failed. Check global.R for errors.")
}

critical_objects <- c("graph_original", "bus_info", "buses_sf", "wfigs_perimeters")
missing_objects <- critical_objects[!sapply(critical_objects, exists)]

if (length(missing_objects) > 0) {
  warning("Missing critical objects: ", paste(missing_objects, collapse = ", "))
}

if (file.exists("WildfireServer.R")) {
  cat("Loading Shiny application...\n")
  source("WildfireServer.R")
  
  if (!exists("ui") || !exists("server")) {
    stop("UI or Server functions not found in server_1.R")
  }
  
  cat("✓ Application ready\n")
  shinyApp(ui = ui, server = server)
} else {
  stop("server not found.")
}