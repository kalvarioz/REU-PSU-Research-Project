# =================================================================================================
# PERSEUS TOPOLOGICAL DATA ANALYSIS - CORRECTED VERSION
# Fixed Net Power Perseus Analysis with Exact M.txt Format Matching
# =================================================================================================

# -------------------------------------------------------------------------------------------------
# REQUIRED LIBRARIES
# -------------------------------------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(TDA)

# -------------------------------------------------------------------------------------------------
# CONFIGURATION - CORRECTED TO MATCH WORKING M.txt FORMAT
# -------------------------------------------------------------------------------------------------
perseus_config <- list(
  # Input/Output paths
  net_power_csv = "databases/net_power_difference_normalizedTEST.csv",
  perseus_exe = normalizePath("Perseus/perseusWin.exe", winslash="\\"),
  outputs_dir = "outputs/",
  
  # Perseus parameters - CORRECTED TO MATCH WORKING VERSION
  perseus_input_file = "M.txt",           # FIXED: was "perseus_distmat.txt"
  perseus_output_prefix = "Moutput",      # FIXED: was "perseus_net_power"
  max_dim = 1,                            # Reduced from 2 to prevent memory issues
  max_scale = 1.0,
  persistence_thresh = 0,
  downsample_max_pts = 300,               # Increased default downsample limit
  
  # Resource management - CRITICAL FIXES
  memory_limit_gb = 32,                    # <-- INCREASED RAM LIMIT TO 32GB
  use_adaptive_sampling = TRUE,
  chunk_processing = FALSE,
  cleanup_temp_files = TRUE,
  
  # Perseus distmat parameters - MATCHING WORKING M.txt: "0 0.1 10 3"
  g = 0,                                  # genus
  s = 0.1,                                # FIXED: was 0.01, now matches working "0.1"
  N = 10,                                 # FIXED: was 50, now matches working "10"
  C = 3,                                  # FIXED: was 2, now matches working "3"
  timeout_seconds = 600                   # <-- INCREASED TIMEOUT to 10 minutes
)

run_full_tda_workflow <- function(fire_data, bus_info, graph_original, 
                                  analysis_radius_miles = 30, simulation_steps = 20) {
  
  message("=== STARTING LOCALIZED TDA WORKFLOW ===")
  fire_name <- unique(fire_data$attr_IncidentName)[1]
  safe_fire_name <- gsub("[^A-Za-z0-9-]", "_", fire_name)
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  
  # 1. Setup Output Directories
  attack_run_dir <- file.path(cfg$outputs_attacked_dir, paste(safe_fire_name, timestamp, sep = "_"))
  before_dir <- file.path(attack_run_dir, "before_analysis_local")
  after_dir <- file.path(attack_run_dir, "after_analysis_local")
  dir.create(attack_run_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(before_dir, showWarnings = FALSE)
  dir.create(after_dir, showWarnings = FALSE)
  message("Results will be saved to: ", attack_run_dir)
  
  # 2. Define Local Area of Interest (AOI) - FIXED VERSION
  message(paste0("[1/6] Defining local analysis area (", analysis_radius_miles, " mile radius)..."))
  
  # Get local area data using the existing function
  local_area_data <- get_local_area_data(fire_data, bus_info, analysis_radius_miles)
  local_bus_data <- local_area_data$buses  # Extract the sf object
  
  # Create local_area_sf for compatibility
  local_area_sf <- local_bus_data
  
  if (nrow(local_bus_data) < 2) {
    return(list(success = FALSE, error = "Not enough buses in the local area to analyze."))
  }
  message("  Found ", nrow(local_bus_data), " buses in the AOI.")
  
  # 3. Analyze "Before" State for the LOCAL AREA
  message("[2/6] Calculating 'Before' power matrix for local area...")
  local_healthy_matrix <- generate_local_power_matrix(local_bus_data)
  message("  Running Perseus on local 'before' state...")
  tda_before_results <- run_perseus_analysis(local_healthy_matrix, output_dir = before_dir)
  if (!tda_before_results$success) {
    return(list(success = FALSE, error = "Perseus failed on local 'before' state."))
  }
  
  # 4. Run Cascade Simulation (on the full grid)
  message("[3/6] Running full-grid wildfire cascade simulation...")
  cascade_results <- run_enhanced_fire_cascade(
    graph = graph_original,
    buses_sf = bus_info, # Use the main bus_info sf object here
    fire_data = fire_data,
    buffer_km = 5,
    steps = simulation_steps
  )
  all_failed_buses_global <- unique(unlist(cascade_results$buses_lost_per_step))
  message("  Simulation complete. Total global buses failed: ", length(all_failed_buses_global))
  
  # 5. Analyze "After" State for the LOCAL AREA
  message("[4/6] Identifying surviving buses within the local area...")
  surviving_local_bus_ids <- setdiff(local_bus_data$bus_i, all_failed_buses_global)
  
  if (length(surviving_local_bus_ids) < 2) {
    return(list(success = FALSE, error = "Not enough surviving buses in the local area to analyze."))
  }
  
  surviving_local_bus_data <- local_bus_data %>% filter(bus_i %in% surviving_local_bus_ids)
  message("  Found ", nrow(surviving_local_bus_data), " surviving local buses.")
  
  message("[5/6] Calculating 'After' power matrix and running Perseus...")
  local_attacked_matrix <- generate_local_power_matrix(surviving_local_bus_data)
  tda_after_results <- run_perseus_analysis(local_attacked_matrix, output_dir = after_dir)
  if (!tda_after_results$success) {
    return(list(success = FALSE, error = "Perseus failed on local 'after' state."))
  }
  
  # 6. Compare and Finalize
  message("[6/6] Comparing local states and saving summary...")
  wasserstein_dist <- calculate_wasserstein_distance(
    tda_before_results$persistence_data,
    tda_after_results$persistence_data
  )
  message("  Local Wasserstein distance (topology change): ", round(wasserstein_dist, 4))
  
  # FIXED: Now local_area_sf is properly defined
  save_analysis_summary(
    output_dir = attack_run_dir, 
    fire_name = fire_name, 
    radius = analysis_radius_miles, 
    failed_global = all_failed_buses_global, 
    local_area_data = local_bus_data, # <-- FIXED: use local_bus_data instead
    attacked_matrix = local_attacked_matrix, 
    before_res = tda_before_results, 
    after_res = tda_after_results, 
    wasserstein = wasserstein_dist
  )
  
  message("✓ Localized TDA Workflow Complete.")
  return(list(success = TRUE, report_path = file.path(attack_run_dir, "analysis_summary.txt")))
}

get_local_area_data <- function(fire_data, buses_sf, radius_miles) {
  # Find the geographic center of the fire event
  fire_center <- st_centroid(st_union(fire_data))
  
  # Convert radius from miles to meters for st_buffer
  radius_meters <- radius_miles * 1609.34
  
  # Create a circular buffer around the fire center
  aoi_buffer <- st_buffer(fire_center, dist = radius_meters)
  
  # Find which buses fall within this buffer
  local_buses_sf <- buses_sf[st_intersects(buses_sf, aoi_buffer, sparse = FALSE), ]
  
  return(list(
    buses = local_buses_sf,
    bus_ids = local_buses_sf$bus_i
  ))
}

# -------------------------------------------------------------------------------------------------
# DATA LOADING FUNCTIONS
# -------------------------------------------------------------------------------------------------

# FIXED: Load matrix with proper error handling
load_net_power_matrix <- function(file_path) {
  message("Loading net power difference matrix from: ", file_path)
  
  if (!file.exists(file_path)) {
    stop("Net power matrix file not found: ", file_path)
  }
  
  # Read the CSV file with error handling
  dt <- tryCatch({
    fread(file_path)
  }, error = function(e) {
    stop("Failed to read CSV file: ", e$message)
  })
  
  if (nrow(dt) == 0) {
    stop("Matrix file is empty")
  }
  
  message("Loaded matrix with dimensions: ", nrow(dt), " x ", ncol(dt))
  
  # Convert to matrix, excluding the first column if it's bus IDs
  if (ncol(dt) > 1) {
    if (names(dt)[1] %in% c("bus_i", "V1") || is.numeric(dt[[1]])) {
      mat <- as.matrix(dt[, -1, with = FALSE])
      rownames(mat) <- dt[[1]]
    } else {
      mat <- as.matrix(dt)
    }
  } else {
    stop("Matrix file appears to have only one column")
  }
  
  # Ensure matrix is square
  if (nrow(mat) != ncol(mat)) {
    warning("Matrix is not square: ", nrow(mat), "x", ncol(mat))
    min_dim <- min(nrow(mat), ncol(mat))
    mat <- mat[1:min_dim, 1:min_dim]
    message("Truncated to square matrix: ", min_dim, "x", min_dim)
  }
  
  # Validate matrix contents
  if (any(is.na(mat))) {
    warning("Matrix contains NA values, replacing with 0")
    mat[is.na(mat)] <- 0
  }
  
  if (any(is.infinite(mat))) {
    warning("Matrix contains infinite values, replacing with max finite value")
    max_finite <- max(mat[is.finite(mat)])
    mat[is.infinite(mat)] <- max_finite
  }
  
  # Check matrix value range
  mat_min <- min(mat, na.rm = TRUE)
  mat_max <- max(mat, na.rm = TRUE)
  mat_mean <- mean(mat, na.rm = TRUE)
  
  message("Matrix value range: [", round(mat_min, 6), ", ", round(mat_max, 6), "]")
  message("Matrix mean: ", round(mat_mean, 6))
  
  # Normalize if needed
  if (mat_max > 2) {
    mat <- mat / mat_max
    message("Matrix normalized to [0,1] range")
  }
  
  return(mat)
}
get_local_buses <- function(fire_data, bus_info, radius_miles) {
  fire_center <- st_centroid(st_union(fire_data))
  radius_meters <- radius_miles * 1609.34
  aoi_buffer <- st_buffer(fire_center, dist = radius_meters)
  
  # CORRECTED: Use the bus_info object for the intersection
  local_buses <- bus_info[st_intersects(bus_info, aoi_buffer, sparse = FALSE), ]
  return(local_buses)
}
generate_local_power_matrix <- function(local_bus_data) {
  # Calculate net power on the fly
  local_bus_power <- local_bus_data %>%
    st_drop_geometry() %>% # Drop spatial data for faster processing
    mutate(net_power = total_gen - load_mw) %>%
    select(bus_i, net_power)
  
  if (nrow(local_bus_power) == 0) return(matrix(nrow = 0, ncol = 0))
  
  power_diff_matrix <- as.matrix(dist(local_bus_power$net_power, method = "manhattan"))
  
  max_diff <- max(power_diff_matrix)
  if (max_diff > 0) {
    power_diff_matrix <- power_diff_matrix / max_diff
  }
  
  return(power_diff_matrix)
}

# -------------------------------------------------------------------------------------------------
# PERSEUS FILE I/O FUNCTIONS
# -------------------------------------------------------------------------------------------------
run_perseus <- function(input_file = NULL, output_prefix = NULL) {
  
  # Use the config from global.R
  if (exists("tda_config") && "simple_perseus" %in% names(tda_config)) {
    cfg <- tda_config$simple_perseus
  } else {
    # Fallback config
    cfg <- list(
      perseus_exe = normalizePath("Perseus/perseusWin.exe", winslash="\\"),
      outputs_dir = "outputs/"
    )
  }
  
  if (is.null(input_file)) {
    input_file <- file.path(cfg$outputs_dir, "M.txt")
  }
  if (is.null(output_prefix)) {
    output_prefix <- file.path(cfg$outputs_dir, "Moutput")
  }
  
  if (!file.exists(cfg$perseus_exe)) {
    stop("Perseus executable not found: ", cfg$perseus_exe)
  }
  
  if (!file.exists(input_file)) {
    stop("Perseus input file not found: ", input_file)
  }
  
  # Run Perseus exactly like working code
  perseus_cmd <- paste(cfg$perseus_exe, "distmat", input_file, output_prefix)
  message("Running Perseus: ", perseus_cmd)
  
  result <- system(perseus_cmd, intern = TRUE)
  
  # Check outputs
  expected_files <- paste0(output_prefix, "_", 0:2, ".txt")
  existing_files <- file.exists(expected_files)
  
  message("Perseus execution complete")
  for (i in seq_along(expected_files)) {
    if (existing_files[i]) {
      size <- file.info(expected_files[i])$size
      message("  ✓ ", basename(expected_files[i]), " (", size, " bytes)")
    }
  }
  
  return(expected_files[existing_files])
}

write_perseus_file <- function(adjacency_matrix, output_dir = NULL) {
  
  # Use config from global.R
  if (exists("tda_config") && "simple_perseus" %in% names(tda_config)) {
    cfg <- tda_config$simple_perseus
  } else {
    # Fallback config
    cfg <- list(
      delta = 0.1, filt_len = 10, cap = 3, genus = 0,
      outputs_dir = "outputs/"
    )
  }
  
  if (is.null(output_dir)) {
    output_dir <- cfg$outputs_dir
  }
  
  # Ensure output directory exists
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Process matrix exactly like working original code
  A2 <- as.matrix(adjacency_matrix)
  A2[A2 == 0] <- 999      # Exact match to working code
  diag(A2) <- 0           # Exact match to working code
  
  d <- nrow(A2)           # Number of points
  
  # Write M.txt file exactly like working version
  output_file <- file.path(output_dir, "M.txt")
  
  # Line 1: number of points  
  cat(d, file = output_file, append = FALSE, sep = '\n')
  
  # Line 2: parameters (g, delta, filt_len, cap)
  cat(paste(cfg$genus, cfg$delta, cfg$filt_len, cfg$cap, sep = ' '), 
      file = output_file, append = TRUE, sep = '\n')
  
  # Line 3+: matrix data (as vector, space-separated)
  cat(as.vector(A2), file = output_file, append = TRUE)
  
  message("✓ Perseus M.txt written: ", output_file, " (", d, "×", d, " matrix)")
  
  return(output_file)
}

# FIXED: Read Perseus outputs matching exact format (Moutput_0.txt, etc.)
read_perseus_outputs <- function(output_prefix = NULL) {
  
  # Use config from global.R
  if (exists("tda_config") && "simple_perseus" %in% names(tda_config)) {
    cfg <- tda_config$simple_perseus
    filt_len <- cfg$filt_len
  } else {
    filt_len <- 10  # Fallback
    cfg <- list(outputs_dir = "outputs/")
  }
  
  if (is.null(output_prefix)) {
    output_prefix <- file.path(cfg$outputs_dir, "Moutput")
  }
  
  # Initialize combined persistence data
  P <- NULL
  
  # Read dimension 0 (exactly like working code)
  dim0_file <- paste0(output_prefix, "_0.txt")
  if (file.exists(dim0_file) && file.info(dim0_file)$size > 0) {
    
    persist_data <- as.matrix(read.table(dim0_file))
    
    # Process exactly like working code
    persist_data[persist_data[, 2] == -1, 2] <- filt_len + 1
    persist_data <- persist_data / (filt_len + 1)
    
    # Add dimension column
    P <- cbind(rep(0, nrow(persist_data)), persist_data)
    
    message("  Dimension 0: ", nrow(persist_data), " features")
  }
  
  # Read dimension 1 (exactly like working code)
  dim1_file <- paste0(output_prefix, "_1.txt") 
  if (file.exists(dim1_file) && file.info(dim1_file)$size > 0) {
    
    persist_data <- as.matrix(read.table(dim1_file, blank.lines.skip = TRUE))
    
    # Process exactly like working code
    persist_data[persist_data[, 2] == -1, 2] <- filt_len + 1
    persist_data <- persist_data / (filt_len + 1)
    
    # Combine
    dim1_data <- cbind(rep(1, nrow(persist_data)), persist_data)
    P <- rbind(P, dim1_data)
    
    message("  Dimension 1: ", nrow(persist_data), " features")
  }
  
  # Set proper column names for TDA compatibility
  if (!is.null(P) && ncol(P) >= 3) {
    colnames(P) <- c("Dimension", "Birth", "Death")
    message("✓ Total features: ", nrow(P))
  } else {
    message("⚠ No persistence features found")
    P <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("Dimension", "Birth", "Death")))
  }
  
  return(P)
}


# -------------------------------------------------------------------------------------------------
# RESULTS SAVING FUNCTIONS
# -------------------------------------------------------------------------------------------------

# Enhanced save function
save_results_tda_fixed <- function(diagram, cfg) {
  message("Saving analysis results...")
  
  features_file <- file.path(cfg$outputs_dir, "persistence_features.csv")
  
  if (nrow(diagram) > 0) {
    df <- as.data.frame(diagram)
    colnames(df) <- c("Dimension", "Birth", "Death")
    df$Persistence <- df$Death - df$Birth
    fwrite(df, features_file)
    message("✓ Persistence features saved: ", features_file)
  }
  
  summary_file <- file.path(cfg$outputs_dir, "analysis_summary.txt")
  
  sink(summary_file)
  cat("=== CORRECTED Net Power Perseus Analysis ===\n")
  cat("Analysis date:", as.character(Sys.time()), "\n")
  cat("Input file:", cfg$net_power_csv, "\n")
  cat("Perseus executable:", cfg$perseus_exe, "\n\n")
  
  cat("Parameters:\n")
  cat("  Max dimension:", cfg$max_dim, "\n")
  cat("  Filtration steps (N):", cfg$N, "\n")
  cat("  Scale increment (s):", cfg$s, "\n")
  cat("  Genus (g):", cfg$g, "\n")
  cat("  Connectivity (C):", cfg$C, "\n")
  cat("  Downsample limit:", cfg$downsample_max_pts, "\n")
  cat("  Timeout (seconds):", cfg$timeout_seconds, "\n\n")
  
  if (nrow(diagram) > 0) {
    cat("Results:\n")
    cat("  Total features:", nrow(diagram), "\n")
    
    df <- as.data.frame(diagram)
    feature_counts <- table(df[,1])
    for (dim in names(feature_counts)) {
      cat("  Dimension", dim, "features:", feature_counts[dim], "\n")
    }
    
    cat("\nMost persistent features:\n")
    df$persistence <- df[,3] - df[,2]
    top_features <- df[order(df$persistence, decreasing = TRUE), ]
    for (i in 1:min(5, nrow(top_features))) {
      cat(sprintf("  %d. Dim %d: Birth=%.4f, Death=%.4f, Persistence=%.4f\n",
                  i, top_features[i,1], top_features[i,2], 
                  top_features[i,3], top_features$persistence[i]))
    }
  } else {
    cat("Results: No persistent features found\n")
  }
  sink()
  
  message("✓ Analysis summary saved: ", summary_file)
}

# -------------------------------------------------------------------------------------------------
# MAIN ANALYSIS FUNCTION
# -------------------------------------------------------------------------------------------------
run_perseus_analysis <- function(adjacency_matrix, output_dir = NULL) {
  message("--- Running Core Perseus TDA ---")
  if (is.null(output_dir)) output_dir <- perseus_config$outputs_dir
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Downsample if matrix is too large
  final_matrix <- downsample_matrix(adjacency_matrix, perseus_config$downsample_max_pts)
  
  tryCatch({
    input_file <- write_perseus_file(final_matrix, output_dir)
    output_prefix <- file.path(output_dir, perseus_config$perseus_output_prefix)
    perseus_cmd <- paste(perseus_config$perseus_exe, "distmat", input_file, output_prefix)
    message("  Executing Perseus with a ", perseus_config$timeout_seconds, " second timeout...")
    system(perseus_cmd, timeout = perseus_config$timeout_seconds)
    persistence_data <- read_perseus_outputs(output_prefix)
    message("✓ Core Perseus TDA complete. Found ", nrow(persistence_data), " features.")
    return(list(success = TRUE, persistence_data = persistence_data, matrix_size = nrow(final_matrix)))
  }, error = function(e) {
    message("✗ Perseus analysis failed: ", e$message)
    return(list(success = FALSE, error = e$message))
  }, warning = function(w) {
    if(grepl("timeout", w$message)) {
      message("✗ Perseus process timed out.")
      return(list(success = FALSE, error = "Perseus timed out."))
    }
  })
}

write_perseus_file <- function(adjacency_matrix, output_dir) {
  output_file <- file.path(output_dir, perseus_config$perseus_input_file)
  d <- nrow(adjacency_matrix)
  cat(d, file = output_file, append = FALSE, sep = '\n')
  cat(paste(perseus_config$g, perseus_config$s, perseus_config$N, perseus_config$C, sep = ' '), file = output_file, append = TRUE, sep = '\n')
  cat(as.vector(adjacency_matrix), file = output_file, append = TRUE)
  return(output_file)
}

read_perseus_outputs <- function(output_prefix) {
  P <- NULL
  filt_len <- perseus_config$N
  for (dim in 0:perseus_config$max_dim) {
    dim_file <- paste0(output_prefix, "_", dim, ".txt")
    if (file.exists(dim_file) && file.info(dim_file)$size > 0) {
      persist_data <- as.matrix(read.table(dim_file, blank.lines.skip = TRUE))
      if (nrow(persist_data) > 0) {
        persist_data[persist_data[, 2] == -1, 2] <- filt_len + 1
        persist_data <- persist_data / (filt_len + 1)
        dim_data <- cbind(rep(dim, nrow(persist_data)), persist_data)
        P <- rbind(P, dim_data)
      }
    }
  }
  if (!is.null(P) && ncol(P) >= 3) {
    colnames(P) <- c("Dimension", "Birth", "Death")
  } else {
    P <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("Dimension", "Birth", "Death")))
  }
  return(P)
}

save_analysis_summary <- function(output_dir, fire_name, radius, failed_global, local_area_data, attacked_matrix, before_res, after_res, wasserstein) {
  summary_file <- file.path(output_dir, "analysis_summary.txt")
  sink(summary_file)
  cat("=== Localized Wildfire Attack TDA Summary ===\n\n")
  cat("Fire Event:", fire_name, "\n")
  cat("Analysis Time:", as.character(Sys.time()), "\n\n")
  cat("--- Analysis Scope ---\n")
  cat("Area of Interest Radius:", radius, "miles\n")
  
  # Fixed: Access nrow properly
  cat("Buses in AOI (Before):", nrow(local_area_data), "\n")
  cat("Buses in AOI (After):", nrow(attacked_matrix), "\n\n")  # This should be the matrix dimensions
  
  cat("--- Global Cascade Summary ---\n")
  cat("Total Buses Failed (Grid-wide):", length(failed_global), "\n\n")
  cat("--- Local TDA Results ---\n")
  cat("Features Before:", nrow(before_res$persistence_data), "\n")
  cat("Features After:", nrow(after_res$persistence_data), "\n")
  cat("Topological Change (Wasserstein):", round(wasserstein, 6), "\n")
  sink()
}

downsample_matrix <- function(mat, max_pts) {
  if (is.null(mat) || nrow(mat) <= max_pts) return(mat)
  set.seed(42)
  keep_indices <- sort(sample(nrow(mat), max_pts))
  message("  Matrix downsampled: ", nrow(mat), " -> ", max_pts, " points")
  return(mat[keep_indices, keep_indices])
}

calculate_wasserstein_distance <- function(P_original, P_current, dimensions = c(0, 1)) {
  if (nrow(P_original) == 0 && nrow(P_current) == 0) return(0)
  if (!requireNamespace("TDA", quietly = TRUE)) stop("TDA package is required.")
  return(TDA::wasserstein(P_original, P_current, dimension = dimensions))
}

plot_persistence_diagram <- function(diagram, title_suffix = "") {
  if (nrow(diagram) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No features found") + theme_minimal())
  }
  df <- as.data.frame(diagram)
  colors <- c("0" = "#1f77b4", "1" = "#ff7f0e", "2" = "#2ca02c")
  ggplot(df, aes(Birth, Death, color = factor(Dimension))) +
    geom_point(size = 3, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed") +
    scale_color_manual(values = colors, name = "Dimension") +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(title = paste("Persistence Diagram", title_suffix)) +
    theme_minimal()
}


extract_bus_ids_from_csv_matrix <- function(matrix_data) {
  # Helper function to extract bus IDs from the loaded CSV matrix
  if (is.null(rownames(matrix_data)) || is.null(colnames(matrix_data))) {
    # If no names, assume sequential bus IDs
    return(1:nrow(matrix_data))
  }
  
  # Extract from row names (handle "bus_123" or "123" formats)
  bus_ids <- as.numeric(gsub("bus_", "", rownames(matrix_data)))
  
  # Remove any NAs and ensure we have valid bus IDs
  bus_ids <- bus_ids[!is.na(bus_ids)]
  
  return(bus_ids)
}

# -------------------------------------------------------------------------------------------------
# SCRIPT INITIALIZATION AND USAGE INFORMATION
# -------------------------------------------------------------------------------------------------

message("=== CORRECTED Perseus V3 Analysis Script Loaded ===")
message("")
message("Key corrections applied:")
message("  ✓ Fixed file names: M.txt and Moutput_X.txt")
message("  ✓ Fixed parameters to match working M.txt: g=0, s=0.1, N=10, C=3")
message("  ✓ Exact matrix format matching working code")
message("  ✓ Better code organization and spacing")
message("  ✓ Enhanced error handling and debugging")

