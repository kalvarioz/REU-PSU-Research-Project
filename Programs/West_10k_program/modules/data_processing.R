# =================================================================================================
# modules/data_processing.R
# 
# Data Processing and Analysis Helper Functions
#
# This module contains functions for:
# - Wildfire data processing
# - Bus proximity calculations  
# - Cascade result summarization
# - TDA result summarization
#
# Brandon Calvario
# =================================================================================================

# ====================================================================
# DATA PROCESSING
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
