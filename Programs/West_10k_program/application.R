# =================================================================================================
# Application.R
# This file launches the Shiny application

# Brandon Calvario


# =================================================================================================

cat("=== Wildfire Grid Resilience Explorer ===\n")

# Initialize system_initialized flag
system_initialized <- FALSE

tryCatch({
  # Load core modules in correct order
  cat("Loading Perseus functions...\n")
  source("TopologicalDataWorkflowWF.R")
  
  cat("Loading Global functions...\n") 
  source("Global.R")
  
  cat("Loading Attack and Cascade functions...\n")
  source("AttackAndCascade.R")
  
  cat("All modules loaded successfully\n")
  
}, error = function(e) {
  cat("Error loading modules: ", e$message, "\n")
  stop("Failed to load required modules")
})

# Verify system initialization
if (!exists("system_initialized") || !system_initialized) {
  cat("System not initialized. Check Global.R for errors.\n")
  cat("Please ensure all data files are present and try again.\n")
  
  # List critical files that should exist
  critical_files <- c(
    "databases/mpc_bus.csv",
    "parsed_csv/bus_data.csv", 
    "databases/mpc_branch.csv",
    "parsed_csv/branch_data.csv",
    "databases/mpc_gen.csv",
    "databases/load_data.csv"
  )
  
  cat("Checking critical files:\n")
  for (file in critical_files) {
    exists <- file.exists(file)
    cat("  ", file, ":", if (exists) "EXISTS" else "MiSSING", "\n")
  }
  
  stop("System initialization failed. Check data files and Global.R configuration.")
}

# Verify critical objects exist
critical_objects <- c("graph_original", "bus_info", "buses_sf", "wfigs_perimeters")
missing_objects <- critical_objects[!sapply(critical_objects, exists)]

if (length(missing_objects) > 0) {
  cat("Missing critical objects: ", paste(missing_objects, collapse = ", "), "\n")
  cat("System may have partial functionality.\n")
}

# Load and launch Shiny application
if (file.exists("WildfireServer.R")) {
  cat("Loading Shiny application...\n")
  source("WildfireServer.R")
  
  if (!exists("ui") || !exists("server")) {
    stop("UI or Server functions not found in WildfireServer.R")
  }
  
  cat("Application ready to launch\n")
  
  # Launch with error handling
  tryCatch({
    shinyApp(ui = ui, server = server)
  }, error = function(e) {
    cat("Error launching Shiny app: ", e$message, "\n")
    cat("Try restarting R and running the application again.\n")
  })
  
} else {
  stop("WildfireServer.R not found.")
}