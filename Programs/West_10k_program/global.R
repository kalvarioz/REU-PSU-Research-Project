# =================================================================================================
# Global.R

# The foundational configuration and data loading engine for the
# entire application. It is executed once at the beginning of the Shiny app's
# lifecycle. Its responsibilities include:

# Brandon Calvario

# =================================================================================================




library(shiny)
library(leaflet)
library(sf)
library(igraph)
library(dplyr)
library(data.table)
library(lubridate)
library(future.apply)
library(maps)
library(TDA)
library(ripserr)
library(TDAstats)
library(ggplot2)
library(tidyr)
library(R6)

setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")

`%||%` <- function(x, y) if (is.null(x)) y else x

monitor_connections <- function() {
  tryCatch({
    all_cons <- showConnections(all = TRUE)
    total_connections <- nrow(all_cons)
    if (total_connections > 10) {
      connection_types <- table(all_cons[, "class"])
      message("High connection count detected: ", total_connections)
      message("Connection types: ", paste(names(connection_types), "=", connection_types, collapse = ", "))
    }
    return(total_connections)
  }, error = function(e) {
    message("Error monitoring connections: ", e$message)
    return(0)
  })
}

cleanup_parallel_resources <- function(force_cleanup = FALSE) {
  message("=== Comprehensive Parallel Cleanup ===")
  tryCatch({
    # Stop future workers
    if (requireNamespace("future", quietly = TRUE)) {
      future::plan(future::sequential)
    }
    # Stop doParallel clusters
    if (requireNamespace("doParallel", quietly = TRUE) && requireNamespace("foreach", quietly = TRUE)) {
      if (foreach::getDoParRegistered()) {
        foreach::registerDoSEQ()
      }
    }
    # Close lingering connections
    all_cons <- showConnections(all = TRUE)
    if (nrow(all_cons) > 3) {
      problematic_cons <- all_cons[all_cons[, "class"] %in% c("textConnection", "sockconn"), , drop = FALSE]
      if (nrow(problematic_cons) > 0) {
        for (i in 1:nrow(problematic_cons)) {
          con_num <- as.integer(rownames(problematic_cons)[i])
          try(close(getConnection(con_num)), silent = TRUE)
        }
      }
    }
    # Reset data.table threads
    if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::setDTthreads(1)
    }
    gc(verbose = FALSE)
    message("✓ Comprehensive parallel cleanup complete.")
  }, error = function(e) {
    message("Error during comprehensive cleanup: ", e$message)
  })
  return(invisible(TRUE))
}

setup_parallel_processing <- function(method = "multisession", max_workers = 4) {
  # Always perform cleanup before setting up a new parallel environment
  cleanup_parallel_resources(force_cleanup = TRUE)
  
  total_cores <- tryCatch({
    parallel::detectCores()
  }, error = function(e) {
    message("Could not detect cores, defaulting to 2.")
    return(2)
  })
  
  # Safely allocate cores, leaving at least one for the system
  n_cores <- max(1, min(max_workers, total_cores - 1))
  
  message("=== SAFE PARALLEL SETUP ===")
  message("Total cores detected: ", total_cores)
  message("Cores allocated for analysis: ", n_cores)
  
  if (total_cores <= 2 || n_cores <= 1) {
    message("Using sequential processing for safety.")
    future::plan(future::sequential)
    return(1)
  }
  
  tryCatch({
    future::plan(future::multisession, workers = n_cores)
    options(future.globals.maxSize = 500 * 1024^2) # 500MB limit
    options(future.rng.onMisuse = "ignore")
    
    if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::setDTthreads(min(2, n_cores))
    }
    
    message("✓ Safe parallel setup complete with ", n_cores, " workers.")
    return(n_cores)
  }, error = function(e) {
    message("Error in parallel setup, falling back to sequential: ", e$message)
    future::plan(future::sequential)
    return(1)
  })
}

run_with_parallel_safety <- function(expr, fallback_expr = NULL, cleanup_after = TRUE) {
  result <- NULL
  tryCatch({
    before_count <- monitor_connections()
    result <- expr
    after_count <- monitor_connections()
    if (after_count > before_count + 5) {
      message("Connection count increased significantly, cleaning up...")
      cleanup_parallel_resources()
    }
  }, error = function(e) {
    message("Error in parallel execution: ", e$message)
    cleanup_parallel_resources(force_cleanup = TRUE)
    
    if (!is.null(fallback_expr)) {
      message("Trying fallback expression...")
      tryCatch({
        result <<- fallback_expr
      }, error = function(e2) {
        message("Fallback also failed: ", e2$message)
        stop("Both parallel and fallback execution failed")
      })
    } else {
      stop(e$message)
    }
  })
  
  if (cleanup_after) {
    Sys.sleep(0.1)
    cleanup_parallel_resources()
  }
  
  return(result)
}

# FIX: Add session cleanup for Shiny
setup_shiny_session_cleanup <- function(session) {
  session$onSessionEnded(function() {
    message("Shiny session ending - cleaning up all resources...")
    cleanup_parallel_resources(force_cleanup = TRUE)
  })
  
  observe({
    invalidateLater(300000) # 5 minutes
    conn_count <- monitor_connections()
    if (conn_count > 15) {
      message("Periodic cleanup triggered due to high connection count")
      cleanup_parallel_resources()
    }
  })
}



cfg <- list(
  data_dir      = "databases/",
  parsed_dir    = "parsed_csv/",
  outputs_dir   = "outputs/",
  outputs_attacked_dir = "outputsAttacked/",
  cache_dir     = "cache/",
  perseus_exe   = normalizePath("Perseus/perseusWin.exe", winslash="\\"),
  western_states = c("washington", "oregon", "california", "idaho", "nevada", 
                     "montana", "wyoming", "utah", "colorado", "arizona", 
                     "new mexico"),
  simulation_steps = 20,
  max_fire_events = 100
)
parallel_processing = list(
  enabled = TRUE,
  method = "multisession",  # Safe for all platforms
  max_workers = max(1, parallel::detectCores() - 1),
  memory_limit_gb = 32  
)


create_output_directories <- function() {
  dirs_to_create <- c(
    cfg$outputs_dir,
    cfg$outputs_attacked_dir,
    cfg$cache_dir, # <-- NEW: Create the cache directory
    "diagnostic_output",
    file.path(tempdir(), "wildfire_analysis")
  )
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      message("  Created directory: ", dir)
    }
  }
}

create_wildfire_cache_structure <- function() {
  cache_dirs <- c(
    file.path(cfg$cache_dir, "wildfire_dates"),
    file.path(cfg$cache_dir, "wildfire_processed"),
    file.path(cfg$cache_dir, "fire_metadata")
  )
  
  sapply(cache_dirs, function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      message("Created cache directory: ", dir)
    }
  })
}

# Generate cache key for wildfire data
generate_wildfire_cache_key <- function(fire_data) {
  # Create a hash based on the fire data content
  key_content <- paste(
    nrow(fire_data),
    ncol(fire_data),
    digest::digest(names(fire_data)),
    if("attr_IncidentName" %in% names(fire_data)) {
      digest::digest(unique(fire_data$attr_IncidentName))
    } else {"no_names"},
    sep = "_"
  )
  return(digest::digest(key_content))
}


get_perseus_config <- function() {
  if (exists("perseus_config")) {
    return(perseus_config)
  } else {
    # Fallback if perseus_V3.R not loaded
    warning("perseus_V3.R not loaded, using fallback config")
    return(list(
      outputs_dir = "outputs/",
      perseus_exe = normalizePath("Perseus/perseusWin.exe", winslash="\\"),
      g = 0, s = 0.1, N = 10, C = 3
    ))
  }
}

load_electrical_data <- function() {
  message("Loading electrical grid data with enhanced validation...")
  
  files_to_check <- list(
    mpc_bus = paste0(cfg$data_dir, "mpc_bus.csv"),
    bus_data = paste0(cfg$parsed_dir, "bus_data.csv"),
    mpc_branch = paste0(cfg$data_dir, "mpc_branch.csv"),
    branch_data = paste0(cfg$parsed_dir, "branch_data.csv"),
    mpc_gen = paste0(cfg$data_dir, "mpc_gen.csv"),
    load_data = paste0(cfg$data_dir, "load_data.csv")
  )
  
  missing_files <- character(0)
  for (name in names(files_to_check)) {
    if (!file.exists(files_to_check[[name]])) {
      missing_files <- c(missing_files, paste0(name, ": ", files_to_check[[name]]))
    }
  }
  
  if (length(missing_files) > 0) {
    stop("Missing critical data files:\n", paste(missing_files, collapse = "\n"))
  }
  
  # ENHANCED: Load mpc_bus with validation
  message("Loading mpc_bus...")
  tryCatch({
    mpc_bus <<- fread(files_to_check$mpc_bus)
    
    # Check if file is empty or corrupted
    if (nrow(mpc_bus) == 0) {
      message("WARNING: mpc_bus.csv is empty, creating minimal structure")
      # Create minimal mpc_bus structure from bus_data
      mpc_bus <<- data.frame(
        bus_i = integer(0),
        type = integer(0),
        pd = numeric(0),
        vm = numeric(0),
        va = numeric(0),
        baseKV = numeric(0),
        zone = integer(0),
        vmax = numeric(0),
        vmin = numeric(0)
      )
    } else {
      # Standard column selection and cleaning
      expected_cols <- c("bus_i", "type", "Pd", "Vm", "Va", "baseKV", "zone", "Vmax", "Vmin")
      available_cols <- intersect(expected_cols, names(mpc_bus))
      
      if (length(available_cols) < 3) {
        message("WARNING: mpc_bus has unexpected structure, using first few columns")
        # Guess column structure
        if (ncol(mpc_bus) >= 9) {
          names(mpc_bus)[1:9] <- c("bus_i", "type", "pd", "vm", "va", "baseKV", "zone", "vmax", "vmin")
        }
      }
      
      mpc_bus <<- mpc_bus %>%
        rename_with(~ case_when(
          .x == "Pd" ~ "pd",
          .x == "Vm" ~ "vm", 
          .x == "Va" ~ "va",
          .x == "Vmax" ~ "vmax",
          .x == "Vmin" ~ "vmin",
          TRUE ~ .x
        )) %>%
        distinct(bus_i, .keep_all = TRUE)
    }
    
    message("mpc_bus loaded: ", nrow(mpc_bus), " rows")
  }, error = function(e) {
    message("ERROR loading mpc_bus: ", e$message)
    # Create empty structure
    mpc_bus <<- data.frame(
      bus_i = integer(0), type = integer(0), pd = numeric(0),
      vm = numeric(0), va = numeric(0), baseKV = numeric(0),
      zone = integer(0), vmax = numeric(0), vmin = numeric(0)
    )
  })
  
  # ENHANCED: Load bus_data with coordinate detection
  message("Loading bus_data with coordinate detection...")
  tryCatch({
    bus_data_raw <- fread(files_to_check$bus_data)
    message("Raw bus_data: ", nrow(bus_data_raw), " rows, ", ncol(bus_data_raw), " columns")
    
    # Detect coordinate columns by examining the data structure
    coord_detection <- detect_coordinate_columns(bus_data_raw)
    
    if (coord_detection$found) {
      message("Detected coordinates in columns: ", coord_detection$lon_col, " (lon), ", coord_detection$lat_col, " (lat)")
      
      bus_data <<- bus_data_raw %>%
        rename(
          bus_i = !!coord_detection$bus_col,
          longitude = !!coord_detection$lon_col,
          latitude = !!coord_detection$lat_col
        ) %>%
        mutate(
          bus_i = as.integer(bus_i),
          longitude = as.numeric(longitude),
          latitude = as.numeric(latitude)
        ) %>%
        # Critical: Remove invalid coordinates
        filter(
          !is.na(bus_i), !is.na(longitude), !is.na(latitude),
          is.finite(longitude), is.finite(latitude),
          abs(longitude) > 0.01, abs(latitude) > 0.01,  # Remove near-zero coordinates
          abs(longitude) <= 180, abs(latitude) <= 90,
          longitude > -130, longitude < -100,  # Reasonable range for western US
          latitude > 30, latitude < 50         # Reasonable range for western US
        ) %>%
        distinct(bus_i, .keep_all = TRUE)
      
      message("bus_data processed: ", nrow(bus_data), " buses with valid coordinates")
      
    } else {
      message("ERROR: Could not detect coordinate columns in bus_data")
      message("Available columns: ", paste(names(bus_data_raw)[1:min(10, ncol(bus_data_raw))], collapse = ", "))
      stop("Cannot proceed without coordinate data")
    }
    
  }, error = function(e) {
    message("CRITICAL ERROR loading bus_data: ", e$message)
    stop("Cannot proceed without bus coordinate data")
  })
  
  # Load other files with existing logic but enhanced error handling
  message("Loading branch data...")
  tryCatch({
    mpc_branch <<- fread(files_to_check$mpc_branch) %>%
      select(from_bus = fbus, to_bus = tbus, r, x, b, rateA, rateB, rateC) %>%
      mutate(b1 = pmin(from_bus, to_bus), b2 = pmax(from_bus, to_bus)) %>%
      distinct(b1, b2, .keep_all = TRUE)
  }, error = function(e) {
    message("ERROR loading mpc_branch: ", e$message)
    mpc_branch <<- data.frame(from_bus = integer(0), to_bus = integer(0), 
                              r = numeric(0), x = numeric(0), b = numeric(0),
                              rateA = numeric(0), rateB = numeric(0), rateC = numeric(0),
                              b1 = integer(0), b2 = integer(0))
  })
  
  tryCatch({
    branch_data <<- fread(files_to_check$branch_data) %>%
      rename(from_bus = BusNum, to_bus = BusNum.1, line_r = LineR, line_x = LineX) %>%
      mutate(
        from_bus = as.integer(from_bus),
        to_bus = as.integer(to_bus),
        b1 = pmin(from_bus, to_bus), 
        b2 = pmax(from_bus, to_bus)
      ) %>%
      distinct(b1, b2, .keep_all = TRUE)
  }, error = function(e) {
    message("ERROR loading branch_data: ", e$message)
    branch_data <<- data.frame(from_bus = integer(0), to_bus = integer(0),
                               line_r = numeric(0), line_x = numeric(0),
                               b1 = integer(0), b2 = integer(0))
  })
  
  tryCatch({
    gen_data <<- fread(files_to_check$mpc_gen) %>%
      rename(bus_i = bus, Pg = Pg, Qg = Qg, Qmax = Qmax, Qmin = Qmin, 
             Vg = Vg, mBase = mBase, status = status, Pmax = Pmax, Pmin = Pmin)
  }, error = function(e) {
    message("ERROR loading gen_data: ", e$message)
    gen_data <<- data.frame(bus_i = integer(0), Pg = numeric(0), Qg = numeric(0))
  })
  
  tryCatch({
    load_data <<- fread(files_to_check$load_data) %>%
      rename(bus_i = BusNum, load_mw = LoadSMW, load_mvar = LoadSMVR)
  }, error = function(e) {
    message("ERROR loading load_data: ", e$message)
    load_data <<- data.frame(bus_i = integer(0), load_mw = numeric(0), load_mvar = numeric(0))
  })
  
  message("✓ Electrical data loading complete")
  message("Final counts:")
  message("  mpc_bus: ", nrow(mpc_bus), " buses")
  message("  bus_data: ", nrow(bus_data), " buses with coordinates")
  message("  mpc_branch: ", nrow(mpc_branch), " branches")
  message("  branch_data: ", nrow(branch_data), " branches")
  message("  gen_data: ", nrow(gen_data), " generators")
  message("  load_data: ", nrow(load_data), " loads")
}
detect_coordinate_columns <- function(data) {
  message("Detecting coordinate columns in data with ", ncol(data), " columns")
  
  # First, identify the bus ID column
  bus_col <- NULL
  possible_bus_cols <- c("BusNum", "bus_i", "Bus", "ID")
  for (col in possible_bus_cols) {
    if (col %in% names(data)) {
      bus_col <- col
      break
    }
  }
  
  if (is.null(bus_col)) {
    # Use first column as bus ID
    bus_col <- names(data)[1]
    message("Using first column as bus ID: ", bus_col)
  }
  
  # Look for longitude/latitude columns by name
  lon_col <- NULL
  lat_col <- NULL
  
  # Check for explicit coordinate column names
  lon_names <- c("longitude", "Longitude", "LONGITUDE", "lon", "LON", "lng", "LNG")
  lat_names <- c("latitude", "Latitude", "LATITUDE", "lat", "LAT")
  
  for (name in lon_names) {
    if (name %in% names(data)) {
      lon_col <- name
      break
    }
  }
  
  for (name in lat_names) {
    if (name %in% names(data)) {
      lat_col <- name
      break
    }
  }
  
  # If not found by name, look for columns with coordinate-like values
  if (is.null(lon_col) || is.null(lat_col)) {
    message("Coordinate columns not found by name, analyzing data patterns...")
    
    # Look for columns with values in typical coordinate ranges
    numeric_cols <- names(data)[sapply(data, function(x) is.numeric(x) || all(grepl("^-?[0-9.]+$", x[!is.na(x)])))]
    
    coord_candidates <- list()
    
    for (col in numeric_cols) {
      if (col == bus_col) next  # Skip bus ID column
      
      values <- as.numeric(data[[col]])
      values <- values[!is.na(values) & is.finite(values)]
      
      if (length(values) > 0) {
        min_val <- min(values)
        max_val <- max(values)
        
        # Check if values look like longitude (western US: -125 to -100)
        if (min_val >= -130 && max_val <= -95 && abs(mean(values)) > 100) {
          coord_candidates$longitude <- c(coord_candidates$longitude, col)
        }
        
        # Check if values look like latitude (western US: 30 to 50)
        if (min_val >= 25 && max_val <= 55 && mean(values) > 25 && mean(values) < 55) {
          coord_candidates$latitude <- c(coord_candidates$latitude, col)
        }
      }
    }
    
    # Select best candidates
    if (length(coord_candidates$longitude) > 0) {
      lon_col <- coord_candidates$longitude[1]
      message("Detected longitude column by pattern: ", lon_col)
    }
    
    if (length(coord_candidates$latitude) > 0) {
      lat_col <- coord_candidates$latitude[1]
      message("Detected latitude column by pattern: ", lat_col)
    }
  }
  
  # Final fallback: look for any two numeric columns that could be coordinates
  if (is.null(lon_col) || is.null(lat_col)) {
    message("Final fallback: using first two numeric columns as coordinates")
    numeric_cols <- names(data)[sapply(data, function(x) is.numeric(x))]
    numeric_cols <- numeric_cols[numeric_cols != bus_col]
    
    if (length(numeric_cols) >= 2) {
      lon_col <- numeric_cols[1]
      lat_col <- numeric_cols[2]
      message("Using ", lon_col, " as longitude and ", lat_col, " as latitude")
    }
  }
  
  found <- !is.null(bus_col) && !is.null(lon_col) && !is.null(lat_col)
  
  return(list(
    found = found,
    bus_col = bus_col,
    lon_col = lon_col,
    lat_col = lat_col
  ))
}

check_file_existence <- function() {
  
  cat("checking file existence\n")
  critical_files <- c(
    paste0(cfg$data_dir, "mpc_bus.csv"),
    paste0(cfg$parsed_dir, "bus_data.csv"),
    paste0(cfg$data_dir, "mpc_branch.csv"),
    paste0(cfg$parsed_dir, "branch_data.csv"),
    paste0(cfg$data_dir, "mpc_gen.csv"),
    paste0(cfg$data_dir, "load_data.csv"),
    "powerworld_resources/10k_bus_grid.gml"
    
  )
  for (file in critical_files) {
    exists <- file.exists(file)
    size <- if (exists) file.info(file)$size else 0
    cat("  ", file, ":", if (exists) "EXISTS" else "MISSING", 
        if (exists) paste0(" (", round(size/1024, 1), " KB)") else "", "\n")
  }
  cat("\nWildfire Data:\n")
  wf_shp <- "WFIGS_Interagency_Perimeters_-8845918407708086874/Perimeters.shp"
  wf_csv <- paste0(cfg$data_dir, "WFIGS_Interagency_Perimeters_-3500393626074286023.csv")
  cat("Shapefile:", if (file.exists(wf_shp)) "EXISTS" else "MISSING", "\n")
  cat("CSV file:", if (file.exists(wf_csv)) "EXISTS" else "MISSING", "\n")
  possible_wf_files <- list.files(pattern = ".*[Pp]erimeter.*\\.(shp|csv)$", recursive = TRUE)
  if (length(possible_wf_files) > 0) {
    cat("  Found potential wildfire files:\n")
    for (f in possible_wf_files) {
      cat("    ", f, "\n")
      
    }
  }
}

diagnose_data_mismatches <- function() {
  
  cat("Data mismatch check:\n\n")
  if (!exists("mpc_bus") || !exists("branch_data")) {
    cat("Data not loaded yet. Run after load_electrical_data()\n")
    
    return()
  }
  cat("1. Basic data count:\n")
  cat("mpc_bus rows:", nrow(mpc_bus), "\n")
  cat("bus_data rows:", nrow(bus_data), "\n") 
  cat("mpc_branch rows:", nrow(mpc_branch), "\n")
  cat("branch_data rows:", nrow(branch_data), "\n\n")
  
  cat("2. Checking for duplictes:\n")
  branch_data_dupes <- branch_data %>%
    mutate(b1 = pmin(from_bus, to_bus), b2 = pmax(from_bus, to_bus)) %>%
    group_by(b1, b2) %>%
    filter(n() > 1) %>%
    ungroup()
  cat("branch_data duplicates (b1,b2 pairs):", nrow(branch_data_dupes), "\n")
  mpc_branch_dupes <- mpc_branch %>%
    mutate(b1 = pmin(from_bus, to_bus), b2 = pmax(from_bus, to_bus)) %>%
    group_by(b1, b2) %>%
    filter(n() > 1) %>%
    ungroup()
  cat("mpc_branch duplicates (b1,b2 pairs):", nrow(mpc_branch_dupes), "\n")
  
}

create_integrated_bus_info <- function() {
  message("Creating integrated bus information with ROW-BASED MERGE strategy...")
  
  # 1. Validate that mpc_bus and bus_data are available and have a similar number of rows
  if (!exists("mpc_bus") || !exists("bus_data") || nrow(mpc_bus) == 0 || nrow(bus_data) == 0) {
    stop("mpc_bus or bus_data is missing or empty. Cannot proceed.")
  }
  if (nrow(mpc_bus) != nrow(bus_data)) {
    message("WARNING: Row count mismatch between mpc_bus (", nrow(mpc_bus), ") and bus_data (", nrow(bus_data), ").")
    message("         Proceeding with the smaller number of rows: ", min(nrow(mpc_bus), nrow(bus_data)))
    min_rows <- min(nrow(mpc_bus), nrow(bus_data))
    mpc_bus <- mpc_bus[1:min_rows, ]
    bus_data <- bus_data[1:min_rows, ]
  }
  
  # 2. Combine the two data frames by column-binding (cbind).
  # This assumes they are in the same order and uses bus_i from mpc_bus as the source of truth.
  bus_info_temp <- cbind(
    mpc_bus %>% select(bus_i, type, pd, vm, va, baseKV, zone, vmax, vmin),
    bus_data %>% select(longitude, latitude)
  ) %>%
    distinct(bus_i, .keep_all = TRUE) # A final safety check for duplicates
  
  message("Combined mpc_bus and bus_data by row order. Result: ", nrow(bus_info_temp), " buses.")
  
  # 3. Join with generation data using the now-reliable bus_i
  if (exists("gen_data") && nrow(gen_data) > 0) {
    gen_summary <- gen_data %>%
      group_by(bus_i) %>%
      summarise(total_gen = sum(Pg, na.rm = TRUE), .groups = 'drop')
    bus_info_temp <- bus_info_temp %>%
      left_join(gen_summary, by = "bus_i") %>%
      mutate(total_gen = ifelse(is.na(total_gen), 0, total_gen))
  } else {
    bus_info_temp$total_gen <- 0
  }
  
  # 4. Join with load data
  if (exists("load_data") && nrow(load_data) > 0) {
    bus_info_temp <- bus_info_temp %>%
      left_join(load_data, by = "bus_i") %>%
      mutate(
        load_mw = ifelse(is.na(load_mw), 0, load_mw),
        load_mvar = ifelse(is.na(load_mvar), 0, load_mvar)
      )
  } else {
    bus_info_temp$load_mw <- 0
    bus_info_temp$load_mvar <- 0
  }
  
  # 5. Create bus type classification
  bus_info_temp <- bus_info_temp %>%
    mutate(
      bus_type = case_when(
        total_gen > 0 & load_mw > 0 ~ "Gen + Load",
        total_gen > 0 ~ "Generator",
        load_mw > 0 ~ "Load",
        TRUE ~ "Neither"
      )
    )
  
  # 6. Validate coordinates before creating sf object
  valid_coords <- bus_info_temp %>%
    filter(
      !is.na(longitude), !is.na(latitude),
      is.finite(longitude), is.finite(latitude),
      abs(longitude) <= 180, abs(latitude) <= 90,
      longitude != 0, latitude != 0
    )
  
  if (nrow(valid_coords) == 0) {
    stop("CRITICAL ERROR: No buses with valid coordinates after merge.")
  }
  
  # 7. Create the final sf object
  tryCatch({
    bus_info <<- st_as_sf(
      valid_coords,
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    )
    buses_sf <<- bus_info
    message("✓ Successfully created sf object with corrected bus IDs: ", nrow(bus_info), " buses")
  }, error = function(e) {
    stop(paste("CRITICAL ERROR during final sf object creation:", e$message))
  })
}


# Diagnostic function to check bus_info status
check_bus_info_status <- function() {
  cat("=== BUS_INFO STATUS CHECK ===\n")
  
  if (!exists("bus_info")) {
    cat("❌ bus_info does not exist\n")
    return(FALSE)
  }
  
  cat("✓ bus_info exists\n")
  cat("  Class: ", paste(class(bus_info), collapse = ", "), "\n")
  cat("  Is sf object: ", inherits(bus_info, "sf"), "\n")
  cat("  Number of rows: ", nrow(bus_info), "\n")
  
  if (inherits(bus_info, "sf")) {
    cat("  CRS: ", st_crs(bus_info)$input, "\n")
    if (nrow(bus_info) > 0) {
      coords <- st_coordinates(bus_info)
      cat("  Coordinate range: X[", round(min(coords[,1]), 3), ",", 
          round(max(coords[,1]), 3), "], Y[", round(min(coords[,2]), 3), 
          ",", round(max(coords[,2]), 3), "]\n")
    }
    cat("✅ bus_info is valid sf object\n")
    return(TRUE)
  } else {
    cat("❌ bus_info is not an sf object\n")
    return(FALSE)
  }
}


create_branch_info <- function() {
  
  message("Creating branch information...")
  branch_info <<- mpc_branch %>%
    
    left_join(branch_data, by = c("b1", "b2")) %>%
    
    # Use mpc_branch values as primary, branch_data as supplementary
    mutate(
      
      r = ifelse(is.na(r), line_r, r),
      
      x = ifelse(is.na(x), line_x, x)
      
    ) %>%
    
    filter(!is.na(r), !is.na(x)) %>%
    distinct(b1, b2, .keep_all = TRUE)
  message("✓ Branch information created: ", nrow(branch_info), " branches")
  
}

load_power_grid <- function() {
  
  message("Loading power grid graph...")
  if (file.exists("powerworld_resources/10k_bus_grid.gml")) {
    
    graph_original <<- read_graph("powerworld_resources/10k_bus_grid.gml", format = "gml") %>%
      as.undirected(mode = "collapse") %>%
      simplify(remove.multiple = TRUE, remove.loops = TRUE)
    message("  Graph loaded from GML: ", vcount(graph_original), " vertices, ", ecount(graph_original), " edges")
    
  } else {
    
    message("  GML file not found, creating graph from branch data...")
    edges_df <- branch_info %>%
      select(from_bus, to_bus, r, x, rateA) %>%
      filter(!is.na(r), !is.na(x)) %>%
      distinct(from_bus, to_bus, .keep_all = TRUE)
    
    if (nrow(edges_df) == 0) {
      
      stop("No valid edges found in branch_info. Check your data loading.")
      
    }
    message("  Creating graph from ", nrow(edges_df), " edges")
    
    graph_original <<- graph_from_data_frame(edges_df, directed = FALSE) %>%
      
      simplify(remove.multiple = TRUE, remove.loops = TRUE)
    message("  Graph created: ", vcount(graph_original), " vertices, ", ecount(graph_original), " edges")
  }
  message("  Adding electrical properties to edges...")
  graph_edges <- igraph::as_data_frame(graph_original, what = "edges") %>%
    mutate(
      from_bus = as.integer(from),
      to_bus = as.integer(to),
      b1 = pmin(from_bus, to_bus), 
      b2 = pmax(from_bus, to_bus)
      
    )
  edges_with_attrs <- graph_edges %>%
    left_join(branch_info %>% select(b1, b2, r, x, rateA), by = c("b1", "b2"))
  edges_with_attrs$r[is.na(edges_with_attrs$r)] <- 0.01
  edges_with_attrs$x[is.na(edges_with_attrs$x)] <- 0.01
  edges_with_attrs$rateA[is.na(edges_with_attrs$rateA)] <- 100
  Y_values <- 1 / (edges_with_attrs$r + 1i * edges_with_attrs$x)
  E(graph_original)$LineR <- edges_with_attrs$r
  E(graph_original)$LineX <- edges_with_attrs$x
  E(graph_original)$RateA <- edges_with_attrs$rateA
  E(graph_original)$Y <- Y_values
  E(graph_original)$Yreal <- Re(Y_values)
  message("✓ Power grid graph loaded successfully")
  message("  Final graph: ", vcount(graph_original), " vertices, ", ecount(graph_original), " edges")
  
}
create_spatial_data <- function() {
  message("Creating spatial data objects...")
  
  # Create states_sf directly - this was the missing piece
  states_sf <<- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
    filter(ID %in% cfg$western_states) %>%
    st_transform(crs = 4326)
  
  message("✓ States spatial data created: ", nrow(states_sf), " western states")
  message("✓ Spatial data objects created")
}
# NEW: Pre-computation function to find fires that can impact the grid
precompute_fire_impact_potential <- function(fires_sf, buses_sf, radius_miles = 30) {
  message("Pre-computing fire impact potential...")
  if (nrow(fires_sf) == 0 || nrow(buses_sf) == 0) {
    message("  Skipping pre-computation: Missing fire or bus data.")
    return(fires_sf %>% mutate(has_impact_potential = FALSE))
  }
  
  # Create a single buffer around all buses
  radius_meters <- radius_miles * 1609.34
  grid_buffer <- st_buffer(st_union(st_geometry(buses_sf)), radius_meters)
  
  # Find which fires intersect this grid-wide buffer
  # This is much faster than checking each fire individually
  potential_fires_indices <- st_intersects(fires_sf, grid_buffer, sparse = FALSE)[,1]
  
  fires_sf <- fires_sf %>%
    mutate(has_impact_potential = potential_fires_indices)
  
  num_potential <- sum(fires_sf$has_impact_potential)
  message("✓ Found ", num_potential, " out of ", nrow(fires_sf), " fires with potential grid impact.")
  
  return(fires_sf)
}

load_wildfire_data <- function() {
  message("Loading wildfire perimeter data...")
  possible_files <- list(
    shp = c("WFIGS_Interagency_Perimeters_-8845918407708086874/Perimeters.shp"),
    csv = c("databases/WFIGS_Interagency_Perimeters_-3500393626074286023.csv")
  )
  existing_files <- list(shp = NULL, csv = NULL)
  for (f in possible_files$shp) {
    if (file.exists(f)) {
      existing_files$shp <- f
      message("  Found shapefile: ", f)
      break
    }
  }
  for (f in possible_files$csv) {
    if (file.exists(f)) {
      existing_files$csv <- f
      message("  Found CSV: ", f)
      break
    }
  }
  if (is.null(existing_files$shp) && is.null(existing_files$csv)) {
    message("  No wildfire files found")
    wfigs_perimeters <<- create_empty_wildfire_data()
    return()
  }
  tryCatch({
    if (!is.null(existing_files$shp)) {
      message("  Loading from shapefile...")
      wfigs_perimeters <<- suppressWarnings(
        
        load_from_shapefile(existing_files$shp, existing_files$csv)
        
      )
      
    } else if (!is.null(existing_files$csv)) {
      message("  Loading from CSV only...")
      wfigs_perimeters <<- load_from_csv_only(existing_files$csv)
      
    }
    wfigs_perimeters <<- process_loaded_wildfire_data(wfigs_perimeters)
    message("✓ Wildfire data loaded: ", nrow(wfigs_perimeters), " perimeters")
    if (nrow(wfigs_perimeters) > 0) {
      show_wildfire_summary()
    }
  }, error = function(e) {
    message("Error loading wildfire data: ", e$message)
    wfigs_perimeters <<- create_empty_wildfire_data()
  })
}

create_empty_wildfire_data <- function() {
  
  st_sf(
    
    attr_IncidentName = character(0),
    
    attr_POOState = character(0),
    
    date_time = as.POSIXct(character(0)),
    
    step = integer(0),
    
    geometry = st_sfc(crs = 4326)
    
  )
  
}
load_from_shapefile <- function(shp_path, csv_path = NULL) {
  geom_data <- st_read(shp_path, quiet = TRUE) %>%
    filter(!st_is_empty(geometry)) %>%
    st_transform(4326)
  message("    Loaded ", nrow(geom_data), " geometries")
  message("    Shapefile columns: ", paste(names(geom_data)[1:min(8, ncol(geom_data))], collapse = ", "))
  if (!is.null(csv_path)) {
    csv_data <- fread(csv_path)
    if (nrow(csv_data) > 0) {
      message("    CSV has ", nrow(csv_data), " records, attempting join...")
      geom_ids <- find_id_columns_in_data(geom_data)
      csv_ids <- find_id_columns_in_data(csv_data)
      if (length(geom_ids) > 0 && length(csv_ids) > 0) {
        geom_id <- geom_ids[1]
        csv_id <- csv_ids[1]
        geom_data[[geom_id]] <- as.character(geom_data[[geom_id]])
        
        csv_data[[csv_id]] <- as.character(csv_data[[csv_id]])
        joined_data <- geom_data %>%
          left_join(csv_data, by = setNames(csv_id, geom_id))
        message("    Joined result: ", nrow(joined_data), " records")
        return(joined_data)
      } else {
        message("    No matching ID columns, using shapefile only")
      }
    } else {
      message("    CSV is empty, using shapefile only")
    }
  }
  return(geom_data)
  
}

load_from_csv_only <- function(csv_path) {
  csv_data <- fread(csv_path)
  if (nrow(csv_data) == 0) {
    message("    CSV file is empty")
    return(create_empty_wildfire_data())
    
  }
  message("    CSV has ", nrow(csv_data), " records")
  return(st_sf(csv_data, geometry = st_sfc(crs = 4326)))
  
}
find_id_columns_in_data <- function(data) {
  col_names <- names(data)
  id_patterns <- c("poly_SourceOID", "poly_Sourc")
  found_ids <- character(0)
  for (pattern in id_patterns) {
    matches <- col_names[grepl(pattern, col_names, ignore.case = TRUE)]
    found_ids <- c(found_ids, matches)
  }
  return(unique(found_ids))
}

process_fuel_and_landowner_data <- function(data) {
  message("    Processing fuel type and landowner data with enhanced cleaning...")
  
  # ====================================================================
  # FUEL TYPE PROCESSING
  # ====================================================================
  
  # Create mapping for primary fuel types (from your images)
  fuel_type_mapping <- c(
    "Chaparral (6 feet)" = "Chaparral",
    "Closed Timber Litter" = "Closed Timber",
    "Dormant Brush, Hardwood Slash" = "Dormant Brush/Hardwood",
    "Hardwood Litter" = "Hardwood Litter",
    "Heavy Logging Slash" = "Heavy Logging Slash",
    "Light Logging Slash" = "Light Logging Slash", 
    "Medium Logging Slash" = "Medium Logging Slash",
    "Short Grass (1 foot)" = "Short Grass",
    "Southern Rough" = "Southern Rough",
    "Tall Grass (2.5 feet)" = "Tall Grass",
    "Timber (Litter and Understory)" = "Timber/Understory",
    "Timber Litter and Understory)" = "Timber/Understory",
    "Brush (2 feet)" = "Brush"
  )
  if ("attr_SecondaryFuelModel" %in% names(data)) {
    data <- data %>% 
      mutate(
        attr_SecondaryFuelModel = dplyr::coalesce(
          as.character(attr_SecondaryFuelModel), "Unknown"
        ),
        fuel_category_secondary = case_when(
          grepl("^(TU|TL)", attr_SecondaryFuelModel, ignore.case = TRUE) ~ "Timber (Litter and Understory)",
          grepl("^GR",       attr_SecondaryFuelModel, ignore.case = TRUE) ~ "Grass",
          grepl("^GS",       attr_SecondaryFuelModel, ignore.case = TRUE) ~ "Grass‑Shrub",
          grepl("^SH",       attr_SecondaryFuelModel, ignore.case = TRUE) ~ "Shrub",
          grepl("^FB",       attr_SecondaryFuelModel, ignore.case = TRUE) ~ "Hardwood",
          grepl("^SB",       attr_SecondaryFuelModel, ignore.case = TRUE) ~ "Slash‑Blowdown",
          grepl("^NB",       attr_SecondaryFuelModel, ignore.case = TRUE) ~ "Non‑Burnable",
          attr_SecondaryFuelModel == "Unknown"                             ~ "Unknown",
          TRUE                                                            ~ "Other"
        )
      )
  } else {
    data$attr_SecondaryFuelModel<- "Unknown"
    data$fuel_category_secondary<- "Unknown"
  }
  # Process Primary Fuel Model (your existing logic)
  if ("attr_PrimaryFuelModel" %in% names(data)) {
    data <- data %>%
      mutate(
        attr_PrimaryFuelModel = case_when(
          is.na(attr_PrimaryFuelModel) | attr_PrimaryFuelModel == "" ~ "Unknown",
          TRUE ~ as.character(attr_PrimaryFuelModel)
        ),
        # Create simplified fuel categories (your existing categories)
        fuel_category = case_when(
          grepl("^(TU|TL)", attr_PrimaryFuelModel, ignore.case = TRUE) ~ "Timber (Litter and Understory)",
          grepl("^GR", attr_PrimaryFuelModel, ignore.case = TRUE) ~ "Grass",
          grepl("^GS", attr_PrimaryFuelModel, ignore.case = TRUE) ~ "Grass-Shrub",
          grepl("^SH", attr_PrimaryFuelModel, ignore.case = TRUE) ~ "Shrub",
          grepl("^FB", attr_PrimaryFuelModel, ignore.case = TRUE) ~ "Hardwood",
          grepl("^SB", attr_PrimaryFuelModel, ignore.case = TRUE) ~ "Slash-Blowdown",
          grepl("^NB", attr_PrimaryFuelModel, ignore.case = TRUE) ~ "Non-Burnable",
          attr_PrimaryFuelModel == "Unknown" ~ "Unknown",
          TRUE ~ "Other"
        )
      )
  } else {
    data$attr_PrimaryFuelModel <- "Unknown"
    data$fuel_category <- "Unknown"
  }
  
  # Process Primary Fuel Type (enhanced with clean names from images)
  if ("attr_PrimaryFuelType" %in% names(data)) {
    data <- data %>%
      mutate(
        # Clean fuel type names
        fuel_type_clean = case_when(
          attr_PrimaryFuelType %in% names(fuel_type_mapping) ~ fuel_type_mapping[attr_PrimaryFuelType],
          is.na(attr_PrimaryFuelType) | attr_PrimaryFuelType == "" ~ "Unknown",
          TRUE ~ as.character(attr_PrimaryFuelType)
        ),
        # Enhanced fuel type categories
        fuel_type_category = case_when(
          grepl("Grass", fuel_type_clean) ~ "Grassland",
          grepl("Timber|Hardwood", fuel_type_clean) ~ "Forest",
          grepl("Slash", fuel_type_clean) ~ "Logging Areas", 
          grepl("Chaparral|Brush", fuel_type_clean) ~ "Shrubland",
          fuel_type_clean %in% c("Southern Rough") ~ "Mixed Vegetation",
          fuel_type_clean == "Unknown" ~ "Unknown",
          TRUE ~ "Other"
        )
      )
  } else {
    data$fuel_type_clean <- "Unknown"
    data$fuel_type_category <- "Unknown"
  }
  
  # ====================================================================
  # LANDOWNER PROCESSING
  # ====================================================================
  
  # Create mapping for detailed landowner types
  detailed_landowner_mapping <- c(
    "ANCSA" = "Alaska Native Corporation",
    "BIA" = "Bureau of Indian Affairs", 
    "BLM" = "Bureau of Land Management",
    "BOR" = "Bureau of Reclamation",
    "County" = "County Government",
    "City" = "Municipal Government", 
    "DOD" = "Department of Defense",
    "DOE" = "Department of Energy",
    "Foreign" = "Foreign Ownership",
    "NPS" = "National Park Service",
    "OthFed" = "Other Federal",
    "OthLoc" = "Other Local Government",
    "Private" = "Private",
    "State" = "State Government",
    "Tribal" = "Tribal Land",
    "USFS" = "US Forest Service",
    "USFWS" = "US Fish & Wildlife Service"
  )
  
  # Process Landowner Category (your existing logic + enhancements)
  if ("attr_POOLandownerCategory" %in% names(data)) {
    data <- data %>%
      mutate(
        attr_POOLandownerCategory = case_when(
          is.na(attr_POOLandownerCategory) | attr_POOLandownerCategory == "" ~ "Unknown",
          TRUE ~ as.character(attr_POOLandownerCategory)
        ),
        landowner_category = case_when(
          grepl("Federal", attr_POOLandownerCategory, ignore.case = TRUE) ~ "Federal",
          grepl("State", attr_POOLandownerCategory, ignore.case = TRUE) ~ "State",
          grepl("Private", attr_POOLandownerCategory, ignore.case = TRUE) ~ "Private",
          grepl("Local", attr_POOLandownerCategory, ignore.case = TRUE) ~ "Local Government",
          grepl("Tribal", attr_POOLandownerCategory, ignore.case = TRUE) ~ "Tribal",
          attr_POOLandownerCategory == "Unknown" ~ "Unknown",
          TRUE ~ "Other"
        ),
        # Enhanced detailed landowner names
        landowner_detailed = case_when(
          attr_POOLandownerCategory %in% names(detailed_landowner_mapping) ~ 
            detailed_landowner_mapping[attr_POOLandownerCategory],
          is.na(attr_POOLandownerCategory) | attr_POOLandownerCategory == "" ~ "Unknown",
          TRUE ~ as.character(attr_POOLandownerCategory)
        ),
        # Enhanced federal vs non-federal grouping
        landowner_federal = case_when(
          attr_POOLandownerCategory %in% c("BLM", "NPS", "USFS", "USFWS", "DOD", "DOE", "BOR", "OthFed") ~ "Federal",
          attr_POOLandownerCategory %in% c("Private") ~ "Private", 
          attr_POOLandownerCategory %in% c("State", "County", "City", "Tribal", "ANCSA") ~ "State/Local/Tribal",
          is.na(attr_POOLandownerCategory) | attr_POOLandownerCategory == "" ~ "Unknown",
          TRUE ~ "Other"
        )
      )
  } else {
    data$attr_POOLandownerCategory <- "Unknown"
    data$landowner_category <- "Unknown"
    data$landowner_detailed <- "Unknown"
    data$landowner_federal <- "Unknown"
  }
  
  # Process Landowner Kind (your existing logic + simple categories from images)
  if ("attr_POOLandownerKind" %in% names(data)) {
    data <- data %>%
      mutate(
        attr_POOLandownerKind = case_when(
          is.na(attr_POOLandownerKind) | attr_POOLandownerKind == "" ~ "Unknown",
          TRUE ~ as.character(attr_POOLandownerKind)
        ),
        # Your existing landowner types
        landowner_type = case_when(
          grepl("Forest Service|USFS", attr_POOLandownerKind, ignore.case = TRUE) ~ "US Forest Service",
          grepl("Park Service|NPS", attr_POOLandownerKind, ignore.case = TRUE) ~ "National Park Service", 
          grepl("BLM|Bureau.*Land", attr_POOLandownerKind, ignore.case = TRUE) ~ "Bureau of Land Management",
          grepl("State", attr_POOLandownerKind, ignore.case = TRUE) ~ "State Agency",
          grepl("Private", attr_POOLandownerKind, ignore.case = TRUE) ~ "Private Owner",
          grepl("County|City|Municipal", attr_POOLandownerKind, ignore.case = TRUE) ~ "Local Government",
          grepl("Tribal|Indian", attr_POOLandownerKind, ignore.case = TRUE) ~ "Tribal Land",
          attr_POOLandownerKind == "Unknown" ~ "Unknown",
          TRUE ~ "Other Agency"
        ),
        #simple categories (from your images: Federal, Other, Private, Blanks)
        landowner_simple = case_when(
          attr_POOLandownerKind == "Federal" ~ "Federal",
          attr_POOLandownerKind == "Private" ~ "Private",
          attr_POOLandownerKind == "Other" ~ "Other",
          is.na(attr_POOLandownerKind) | attr_POOLandownerKind == "" ~ "Unknown",
          TRUE ~ as.character(attr_POOLandownerKind)
        )
      )
  } else {
    data$attr_POOLandownerKind <- "Unknown" 
    data$landowner_type <- "Unknown"
    data$landowner_simple <- "Unknown"
  }
  
  return(data)
}

standardize_wildfire_columns <- function(data) {
  # Complete mapping including the REAL WFIGS column names you have
  col_mapping <- list(
    attr_IncidentName = c("poly_IncidentName", "poly_Incid", "IncidentName"),
    attr_POOState = c("attr_POOState", "attr_POOst", "POOState"),
    poly_CreateDate = c("poly_CreateDate", "poly_Creat", "CreateDate"),
    poly_PolygonDateTime = c("poly_PolygonDateTime", "poly_Polyg", "PolygonDateTime"),
    poly_SourceOID = c("poly_SourceOID", "poly_Sourc", "SourceOID"),
    attr_InitialLatitude = c("attr_InitialLatitude", "attr_Initi", "InitialLatitude"),
    attr_InitialLongitude = c("attr_InitialLongitude", "attr_Ini_1", "InitialLongitude"),
    Shape__Area = c("Shape__Area", "Shape_Area", "area"),
    Shape__Length = c("Shape__Length", "Shape_Length", "perimeter"),
    poly_Acres_AutoCalc = c("poly_Acres_AutoCalc", "poly_Acres", "Acres_AutoCalc", "acres"),
    attr_ICS209ReportDateTime = c("attr_ICS209ReportDateTime", "attr_ICS209RptDateTime"),
    attr_ICS209RptForTimePeriodFrom = c("attr_ICS209RptForTimePeriodFrom"),
    attr_ICS209RptForTimePeriodTo = c("attr_ICS209RptForTimePeriodTo"),
    attr_PrimaryFuelModel = c("attr_PrimaryFuelModel"),
    attr_SecondaryFuelModel = c("attr_SecondaryFuelModel"),
    attr_POOLandownerCategory = c("attr_POOLandownerCategory"),
    attr_POOLandownerKind = c("attr_POOLandownerKind")
  )
  
  for (standard_name in names(col_mapping)) {
    possible_names <- col_mapping[[standard_name]]
    for (possible_name in possible_names) {
      if (possible_name %in% names(data) && !standard_name %in% names(data)) {
        data[[standard_name]] <- data[[possible_name]]
        message("Mapped ", possible_name, " -> ", standard_name)
        break
      }
    }
  }
  
  # Ensure numeric columns are properly typed
  numeric_cols <- c("attr_InitialLatitude", "attr_InitialLongitude", "poly_Acres_AutoCalc")
  for (col in numeric_cols) {
    if (col %in% names(data)) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  # Convert state codes to full names
  if ("attr_POOState" %in% names(data)) {
    data <- data %>%
      mutate(
        attr_POOState = case_when(
          attr_POOState == "US-CA" ~ "california",
          attr_POOState == "US-OR" ~ "oregon", 
          attr_POOState == "US-WA" ~ "washington",
          attr_POOState == "US-ID" ~ "idaho",
          attr_POOState == "US-NV" ~ "nevada",
          attr_POOState == "US-MT" ~ "montana",
          attr_POOState == "US-WY" ~ "wyoming",
          attr_POOState == "US-UT" ~ "utah",
          attr_POOState == "US-CO" ~ "colorado",
          attr_POOState == "US-AZ" ~ "arizona",
          attr_POOState == "US-NM" ~ "new mexico",
          tolower(attr_POOState) %in% cfg$western_states ~ tolower(attr_POOState),
          TRUE ~ tolower(attr_POOState)
        )
      )
  }
  
  # Process fuel and landowner data
  data <- process_fuel_and_landowner_data(data)
  
  return(data)
}

add_date_and_step_info <- function(data) {
  message("Processing date and step information with REAL WFIGS columns...")
  
  # PRIORITY 1: Use ICS209 time period columns if available (REAL DATA)
  has_ics209_periods <- "attr_ICS209RptForTimePeriodFrom" %in% names(data) && 
    "attr_ICS209RptForTimePeriodTo" %in% names(data)
  
  if (has_ics209_periods) {
    message("      Found ICS209 time period columns - using REAL fire reporting periods")
    
    data <- data %>%
      mutate(
        # Convert ICS209 time periods to proper datetime
        period_from = tryCatch({
          from_strings <- as.character(attr_ICS209RptForTimePeriodFrom)
          parsed_from <- lubridate::parse_date_time(
            from_strings,
            orders = c("Ymd HMS", "mdY HMS", "m/d/Y I:M:S p", 
                       "Ymd", "mdY", "Y-m-d", "Y/m/d", "m/d/Y"),
            quiet = TRUE
          )
          
          # Handle cases where parsing fails
          if (all(is.na(parsed_from))) {
            # Try as POSIXct
            parsed_from <- as.POSIXct(from_strings, tz = "UTC")
          }
          
          parsed_from
        }, error = function(e) {
          message("Error parsing period_from: ", e$message)
          as.POSIXct(attr_ICS209RptForTimePeriodFrom, tz = "UTC")
        }),
        
        period_to = tryCatch({
          to_strings <- as.character(attr_ICS209RptForTimePeriodTo)
          parsed_to <- lubridate::parse_date_time(
            to_strings,
            orders = c("Ymd HMS", "mdY HMS", "m/d/Y I:M:S p", 
                       "Ymd", "mdY", "Y-m-d", "Y/m/d", "m/d/Y"),
            quiet = TRUE
          )
          
          # Handle cases where parsing fails
          if (all(is.na(parsed_to))) {
            # Try as POSIXct
            parsed_to <- as.POSIXct(to_strings, tz = "UTC")
          }
          
          parsed_to
        }, error = function(e) {
          message("        Error parsing period_to: ", e$message)
          as.POSIXct(attr_ICS209RptForTimePeriodTo, tz = "UTC")
        })
      )
    
    # Count how many fires have valid ICS209 data
    valid_periods <- !is.na(data$period_from) & !is.na(data$period_to)
    fires_with_periods <- length(unique(data$attr_IncidentName[valid_periods]))
    fires_without_periods <- length(unique(data$attr_IncidentName[!valid_periods]))
    
    message("ICS209 time period data summary:")
    message("Fires WITH valid periods: ", fires_with_periods)
    message("Fires WITHOUT valid periods: ", fires_without_periods)
    message("Total records with valid periods: ", sum(valid_periods), "/", nrow(data))
  }
  
  # PRIORITY 2: Use standard polygon date columns as fallback
  date_columns <- c("poly_PolygonDateTime", "poly_Polyg", "poly_CreateDate", "poly_Creat", "CreateDate")
  date_col <- NULL
  
  for (col in date_columns) {
    if (col %in% names(data) && !all(is.na(data[[col]]))) {
      date_col <- col
      break
    }
  }
  
  if (!is.null(date_col)) {
    message("      Using fallback date column: ", date_col)
    
    data <- data %>%
      mutate(
        date_time = tryCatch({
          date_strings <- as.character(.data[[date_col]])
          
          # Try multiple date formats
          parsed_dates <- lubridate::parse_date_time(
            date_strings,
            orders = c("Ymd HMS", "mdY HMS", "m/d/Y I:M:S p", 
                       "Ymd", "mdY", "Y-m-d", "Y/m/d", "m/d/Y"),
            quiet = TRUE
          )
          
          # Fallback for failed parsing
          if (all(is.na(parsed_dates))) {
            parsed_dates <- as.POSIXct(date_strings, 
                                       format = "%Y-%m-%d %H:%M:%S", 
                                       tz = "UTC")
          }
          
          # Final fallback
          if (all(is.na(parsed_dates))) {
            rep(Sys.time(), length(date_strings))
          } else {
            parsed_dates
          }
          
        }, error = function(e) {
          message("Date parsing error: ", e$message)
          rep(Sys.time(), nrow(data))
        }),
        
        date_only = as.Date(date_time)
      )
  } else {
    message("No standard date columns found, using current time")
    data$date_time <- Sys.time()
    data$date_only <- Sys.Date()
  }
  
#Use ICS209 data where available, fallback for others
  if (has_ics209_periods) {
    data <- data %>%
      mutate(
        # Use ICS209 periods where available, fallback to date_time
        period_from = case_when(
          !is.na(period_from) ~ period_from,
          !is.na(date_time) ~ date_time,
          TRUE ~ Sys.time()
        ),
        
        period_to = case_when(
          !is.na(period_to) ~ period_to,
          !is.na(period_from) ~ period_from + hours(24),
          !is.na(date_time) ~ date_time + hours(24),
          TRUE ~ Sys.time() + hours(24)
        )
      )
  } else {
    # No ICS209 data available, use date_time for all
    data <- data %>%
      mutate(
        period_from = date_time,
        period_to = date_time + hours(24)
      )
  }
  
  # Calculate period duration and quality
  data <- data %>%
    mutate(
      period_duration_hours = as.numeric(difftime(period_to, period_from, units = "hours")),
      
      # Fix invalid durations
      period_duration_hours = case_when(
        is.na(period_duration_hours) ~ 24,
        period_duration_hours <= 0 ~ 24,
        period_duration_hours > 8760 ~ 24,  # More than a year
        TRUE ~ period_duration_hours
      ),
      
      # Assess data quality based on source
      time_period_quality = case_when(
        has_ics209_periods & !is.na(attr_ICS209RptForTimePeriodFrom) & !is.na(attr_ICS209RptForTimePeriodTo) ~ "high",
        !is.na(date_time) ~ "medium",
        TRUE ~ "low"
      ),
      
      # Identify gap periods (likely missing reports)
      is_gap_period = period_duration_hours > 48  # More than 2 days suggests missing reports
    )
  
  # Create steps based on incident progression WITH SMART GROUPING
  if ("attr_IncidentName" %in% names(data)) {
    data <- data %>%
      group_by(attr_IncidentName) %>%
      arrange(period_from) %>%
      mutate(
        step = tryCatch({
          # For fires with high-quality time data, use actual time periods
          if (any(time_period_quality == "high")) {
            # Use actual time periods to create steps
            unique_periods <- sort(unique(period_from[!is.na(period_from)]))
            step_values <- match(period_from, unique_periods)
            step_values[is.na(step_values)] <- 1L
            as.integer(step_values)
          } else {
            # For fires without good time data, use simple sequential steps
            seq_len(n())
          }
        }, error = function(e) {
          message("        Step creation error for ", attr_IncidentName[1], ": ", e$message)
          seq_len(n())
        })
      ) %>%
      ungroup()
  } else {
    data$step <- 1L
  }
  
  # Final validation and cleanup
  required_columns <- c("step", "period_from", "period_to", "period_duration_hours", 
                        "time_period_quality", "is_gap_period")
  
  for (col in required_columns) {
    if (!col %in% names(data)) {
      message("      Creating missing column: ", col)
      data[[col]] <- switch(col,
                            "step" = 1L,
                            "period_from" = Sys.time(),
                            "period_to" = Sys.time() + hours(24),
                            "period_duration_hours" = 24,
                            "time_period_quality" = "low",
                            "is_gap_period" = FALSE
      )
    }
  }
  
  # Final cleanup
  data$step[is.na(data$step) | data$step < 1] <- 1L
  data$period_duration_hours[is.na(data$period_duration_hours) | data$period_duration_hours <= 0] <- 24
  data$time_period_quality[is.na(data$time_period_quality)] <- "low"
  data$is_gap_period[is.na(data$is_gap_period)] <- FALSE
  
  # Summary statistics
  if (has_ics209_periods) {
    high_quality_fires <- length(unique(data$attr_IncidentName[data$time_period_quality == "high"]))
    message("FINAL SUMMARY:")
    message("Fires with HIGH quality time data: ", high_quality_fires)
    message("Steps range: ", min(data$step, na.rm = TRUE), " to ", max(data$step, na.rm = TRUE))
    message("Average period duration: ", round(mean(data$period_duration_hours, na.rm = TRUE), 1), " hours")
  } else {
    message("Date processing complete with fallback data")
    message("Steps range: ", min(data$step, na.rm = TRUE), " to ", max(data$step, na.rm = TRUE))
  }
  
  return(data)
}

clean_duplicate_data <- function() {
  message("Cleaning duplicate data entries...")
  
  # Clean bus data duplicates
  if (exists("bus_info") && nrow(bus_info) > 0) {
    original_count <- nrow(bus_info)
    bus_info <<- bus_info %>% distinct(bus_i, .keep_all = TRUE)
    if (nrow(bus_info) < original_count) {
      message("Removed ", original_count - nrow(bus_info), " duplicate bus entries")
    }
  }
  
  # Clean mpc_bus duplicates
  if (exists("mpc_bus") && nrow(mpc_bus) > 0) {
    original_count <- nrow(mpc_bus)
    mpc_bus <<- mpc_bus %>% distinct(bus_i, .keep_all = TRUE)
    if (nrow(mpc_bus) < original_count) {
      message("Removed ", original_count - nrow(mpc_bus), " duplicate mpc_bus entries")
    }
  }
  
  # Clean bus_data duplicates
  if (exists("bus_data") && nrow(bus_data) > 0) {
    original_count <- nrow(bus_data)
    bus_data <<- bus_data %>% distinct(bus_i, .keep_all = TRUE)
    if (nrow(bus_data) < original_count) {
      message("Removed ", original_count - nrow(bus_data), " duplicate bus_data entries")
    }
  }
  
  # Clean branch data duplicates
  if (exists("branch_info") && nrow(branch_info) > 0) {
    original_count <- nrow(branch_info)
    branch_info <<- branch_info %>% distinct(b1, b2, .keep_all = TRUE)
    if (nrow(branch_info) < original_count) {
      message("Removed ", original_count - nrow(branch_info), " duplicate branch entries")
    }
  }
  
  # Clean mpc_branch duplicates
  if (exists("mpc_branch") && nrow(mpc_branch) > 0) {
    original_count <- nrow(mpc_branch)
    mpc_branch <<- mpc_branch %>% distinct(b1, b2, .keep_all = TRUE)
    if (nrow(mpc_branch) < original_count) {
      message("Removed ", original_count - nrow(mpc_branch), " duplicate mpc_branch entries")
    }
  }
  
  # Clean branch_data duplicates
  if (exists("branch_data") && nrow(branch_data) > 0) {
    original_count <- nrow(branch_data)
    branch_data <<- branch_data %>% distinct(b1, b2, .keep_all = TRUE)
    if (nrow(branch_data) < original_count) {
      message("Removed ", original_count - nrow(branch_data), " duplicate branch_data entries")
    }
  }
  
  message("--Data cleaning complete")
  return(invisible(TRUE))
}

classify_fire_intensity <- function(data) {
  acres_col <- NULL
  if ("poly_Acres_AutoCalc" %in% names(data)) {
    acres_col <- "poly_Acres_AutoCalc"
  } else if ("poly_Acres" %in% names(data)) {
    acres_col <- "poly_Acres"
  }
  
  if (!is.null(acres_col)) {
    data <- data %>%
      mutate(
        fire_acres = as.numeric(.data[[acres_col]]),
        fire_acres = ifelse(is.na(fire_acres), 0, fire_acres),
        fire_intensity = case_when(
          fire_acres >= 50000 ~ "Extreme",
          fire_acres >= 10000 ~ "High",
          fire_acres >= 1000 ~ "Moderate",
          fire_acres >= 100 ~ "Low",
          fire_acres > 0 ~ "Very Low",
          TRUE ~ "Unknown"
        ),
        fire_size_category = case_when(
          fire_acres >= 100000 ~ "Megafire",
          fire_acres >= 50000 ~ "Large Fire",
          fire_acres >= 10000 ~ "Significant Fire",
          fire_acres >= 1000 ~ "Standard Fire",
          fire_acres > 0 ~ "Small Fire",
          TRUE ~ "Unknown"
        )
      )
  } else {
    data$fire_intensity <- "Unknown"
    data$fire_size_category <- "Unknown"
    data$fire_acres <- 0
  }
  
  return(data)
}

create_fire_center_points <- function(data) {
  # Check for coordinate columns - both full and truncated names
  lat_col <- NULL
  lon_col <- NULL
  
  if ("attr_InitialLatitude" %in% names(data)) {
    lat_col <- "attr_InitialLatitude"
    lon_col <- "attr_InitialLongitude"
  } else if ("attr_Initi" %in% names(data)) {
    data$attr_InitialLatitude <- as.numeric(data$attr_Initi)
    data$attr_InitialLongitude <- as.numeric(data$attr_Ini_1)
    lat_col <- "attr_InitialLatitude"
    lon_col <- "attr_InitialLongitude"
  }
  
  if (!is.null(lat_col) && !is.null(lon_col)) {
    data[[lat_col]] <- as.numeric(data[[lat_col]])
    data[[lon_col]] <- as.numeric(data[[lon_col]])
    
    valid_coords <- !is.na(data[[lat_col]]) & 
      !is.na(data[[lon_col]]) &
      abs(data[[lat_col]]) > 0 & 
      abs(data[[lon_col]]) > 0 &
      abs(data[[lat_col]]) <= 90 &
      abs(data[[lon_col]]) <= 180
    
    if (sum(valid_coords) > 0) {
      message(" ** Found ", sum(valid_coords), " fires with initial coordinates")
      data$has_center_point <- valid_coords
    } else {
      data$has_center_point <- FALSE
    }
  } else {
    data$has_center_point <- FALSE
    data$attr_InitialLatitude <- NA
    data$attr_InitialLongitude <- NA
  }
  
  return(data)
}

process_loaded_wildfire_data <- function(data) {
  message("Processing wildfire data with enhanced features...")
  
  data <- standardize_wildfire_columns(data)
  
  # Filter by western states
  if ("attr_POOState" %in% names(data)) {
    before_count <- nrow(data)
    data <- data %>%
      filter(tolower(attr_POOState) %in% cfg$western_states | is.na(attr_POOState))
    after_count <- nrow(data)
    message("Filtered by western states: ", before_count, " -> ", after_count)
  }
  
  # UPDATED: Use enhanced time period processing
  data <- add_date_and_step_info(data)  # Use new function
  data <- classify_fire_intensity(data)
  data <- create_fire_center_points(data)
  
  # Fix any geometry issues
  if (inherits(data, "sf") && any(!st_is_valid(data))) {
    message("Fixing invalid geometries...")
    data <- st_make_valid(data)
  }
  
  return(data)
}

find_fires_affecting_grid <- function(fire_data, buses_sf, buffer_km = 5) {
  
  message("Analyzing fire-grid intersections...")
  if (nrow(fire_data) == 0 || nrow(buses_sf) == 0) {
    return(list(
      affected_buses = integer(0),
      fire_bus_intersections = data.frame(),
      fire_impact_summary = data.frame()
    ))
  }
  # Convert buffer to degrees (approximate)
  buffer_deg <- buffer_km / 111  # Rough conversion
  # Find direct intersections with fire perimeters
  direct_hits <- st_within(buses_sf, fire_data)
  direct_affected_buses <- buses_sf$bus_i[lengths(direct_hits) > 0]
  # Find buses within buffer of fire center points
  buffer_affected_buses <- integer(0)
  if ("has_center_point" %in% names(fire_data)) {
    fires_with_centers <- fire_data %>%
      filter(has_center_point) %>%
      st_drop_geometry()
    if (nrow(fires_with_centers) > 0) {
      # Create points from fire centers
      fire_centers_sf <- fires_with_centers %>%
        filter(!is.na(attr_InitialLatitude), !is.na(attr_InitialLongitude)) %>%
        st_as_sf(coords = c("attr_InitialLongitude", "attr_InitialLatitude"),
                 crs = 4326)
      if (nrow(fire_centers_sf) > 0) {
        # Create buffers around fire centers
        fire_buffers <- st_buffer(fire_centers_sf, dist = buffer_deg)
        # Find buses within buffers
        buffer_hits <- st_within(buses_sf, fire_buffers)
        buffer_affected_buses <- buses_sf$bus_i[lengths(buffer_hits) > 0]
      }
    }
  }
  all_affected_buses <- unique(c(direct_affected_buses, buffer_affected_buses))
  # Create detailed intersection data
  fire_bus_intersections <- data.frame()
  if (length(all_affected_buses) > 0) {
    affected_buses_sf <- buses_sf %>% filter(bus_i %in% all_affected_buses)
    # For each affected bus, find which fires affect it
    for (bus_id in all_affected_buses) {
      bus_point <- buses_sf[buses_sf$bus_i == bus_id, ]
      # Check direct intersection
      direct_fire_hits <- st_within(bus_point, fire_data)
      direct_fires <- if (length(direct_fire_hits[[1]]) > 0) {
        fire_data[direct_fire_hits[[1]], ] %>% st_drop_geometry()
      } else {
        data.frame()
      }
      if (nrow(direct_fires) > 0) {
        
        for (j in 1:nrow(direct_fires)) {
          
          fire_bus_intersections <- rbind(fire_bus_intersections, data.frame(
            bus_i = bus_id,
            fire_name = direct_fires$attr_IncidentName[j],
            intersection_type = "direct",
            distance_km = 0,
            fire_size_acres = if ("poly_Acres_AutoCalc" %in% names(direct_fires)) direct_fires$poly_Acres_AutoCalc[j] else NA,
            fire_intensity = if ("fire_intensity" %in% names(direct_fires)) direct_fires$fire_intensity[j] else "Unknown"
            
          ))
        }
      }
    }
  }
  fire_impact_summary <- data.frame()
  if (nrow(fire_bus_intersections) > 0) {
    fire_impact_summary <- fire_bus_intersections %>%
      group_by(fire_name, fire_intensity) %>%
      summarise(
        buses_affected = n_distinct(bus_i),
        direct_hits = sum(intersection_type == "direct"),
        avg_distance = mean(distance_km, na.rm = TRUE),
        max_fire_size = max(fire_size_acres, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(buses_affected))
  }
  message("  Found ", length(all_affected_buses), " affected buses")
  message("  Direct hits: ", length(direct_affected_buses))
  message("  Buffer hits: ", length(buffer_affected_buses))
  return(list(
    affected_buses = all_affected_buses,
    fire_bus_intersections = fire_bus_intersections,
    fire_impact_summary = fire_impact_summary
  ))
}

initialize_healthy_state_baseline <- function() {
  message("=== INITIALIZING HEALTHY STATE BASELINE ===")
  csv_file <- "databases/net_power_difference_normalizedTEST.csv"
  
  if (!file.exists(csv_file)) {
    message("CSV file not found: ", csv_file)
    return(list(success = FALSE, error = "CSV file missing"))
  }
  tryCatch({
    healthy_matrix <- load_net_power_matrix(csv_file)
    # Store globally for all future use
    assign("healthy_state_matrix", healthy_matrix, envir = .GlobalEnv)
    
    message("✓ Healthy state baseline established FROM ORIGINAL MATRIX")
    message("  Matrix size: ", nrow(healthy_matrix), "×", ncol(healthy_matrix))
    message("  Value range: [", round(min(healthy_matrix, na.rm = TRUE), 6), 
            ", ", round(max(healthy_matrix, na.rm = TRUE), 6), "]")
    message("  Matrix will be used as-is for all TDA analysis")
    
    return(list(
      success = TRUE,
      matrix = healthy_matrix,
      note = "Matrix loaded from pre-normalized CSV without additional processing"
    ))
    
  }, error = function(e) {
    message("✗ Failed to initialize healthy state: ", e$message)
    return(list(success = FALSE, error = e$message))
  })
}

show_wildfire_summary <- function() {
  if ("attr_POOState" %in% names(wfigs_perimeters)) {
    state_counts <- table(wfigs_perimeters$attr_POOState, useNA = "ifany")
    message("  Records by state:")
    for (state in names(state_counts)) {
      if (!is.na(state) && state_counts[state] > 0) {
        message("    ", state, ": ", state_counts[state])
      }
    }
    if ("NA" %in% names(state_counts) && state_counts["NA"] > 0) {
      message("    Unassigned: ", state_counts["NA"])
    }
  }
  
  if ("attr_IncidentName" %in% names(wfigs_perimeters)) {
    unique_fires <- length(unique(wfigs_perimeters$attr_IncidentName))
    message("  Unique fire incidents: ", unique_fires)
    
    # Show top 5 fire names
    top_fires <- wfigs_perimeters %>%
      count(attr_IncidentName, sort = TRUE) %>%
      head(5)
    if (nrow(top_fires) > 0) {
      message("  Most frequent fires:")
      for (i in 1:nrow(top_fires)) {
        message("    ", top_fires$attr_IncidentName[i], ": ", top_fires$n[i], " perimeters")
      }
    }
  }
  
  if ("poly_PolygonDateTime" %in% names(wfigs_perimeters) && !all(is.na(wfigs_perimeters$date_time))) {
    date_range <- range(wfigs_perimeters$date_time, na.rm = TRUE)
    message("  Date range: ", as.Date(date_range[1]), " to ", as.Date(date_range[2]))
  }
  if ("fuel_category" %in% names(wfigs_perimeters)) {
    fuel_summary <- table(wfigs_perimeters$fuel_category, useNA = "ifany")
    message("  Fuel types:")
    for (fuel in names(fuel_summary)) {
      if (!is.na(fuel) && fuel_summary[fuel] > 0) {
        message("    ", fuel, ": ", fuel_summary[fuel])
      }
    }
  }
  
  if ("landowner_category" %in% names(wfigs_perimeters)) {
    landowner_summary <- table(wfigs_perimeters$landowner_category, useNA = "ifany")
    message("  Landowner categories:")
    for (owner in names(landowner_summary)) {
      if (!is.na(owner) && landowner_summary[owner] > 0) {
        message("    ", owner, ": ", landowner_summary[owner])
      }
    }
  }
}
create_global_color_palettes <- function() {
  create_color_palettes()
}
get_filter_options_for_selection <- function(state = NULL, intensity = NULL) {
  if (!exists("wfigs_perimeters") || nrow(wfigs_perimeters) == 0) {
    return(list(
      fuel_types = "No data",
      fuel_categories = "No data", 
      landowner_categories = "No data",
      landowner_types = "No data"
    ))
  }
  
  # Filter data based on selections
  filtered_data <- wfigs_perimeters
  
  if (!is.null(state)) {
    filtered_data <- filtered_data %>%
      filter(tolower(attr_POOState) == tolower(state))
  }
  
  if (!is.null(intensity)) {
    filtered_data <- filtered_data %>%
      filter(fire_intensity == intensity)
  }
  
  if (nrow(filtered_data) == 0) {
    return(list(
      fuel_types = "No data",
      fuel_categories = "No data",
      landowner_categories = "No data", 
      landowner_types = "No data"
    ))
  }
  
  # Extract unique values
  options <- list(
    fuel_types = sort(unique(filtered_data$attr_PrimaryFuelModel[!is.na(filtered_data$attr_PrimaryFuelModel) & filtered_data$attr_PrimaryFuelModel != ""])),
    fuel_categories = sort(unique(filtered_data$fuel_category[!is.na(filtered_data$fuel_category) & filtered_data$fuel_category != ""])),
    landowner_categories = sort(unique(filtered_data$landowner_category[!is.na(filtered_data$landowner_category) & filtered_data$landowner_category != ""])),
    landowner_types = sort(unique(filtered_data$landowner_type[!is.na(filtered_data$landowner_type) & filtered_data$landowner_type != ""]))
  )
  
  # Handle empty results
  options <- lapply(options, function(x) {
    if (length(x) == 0) x <- "No data"
    return(x)
  })
  
  return(options)
}

# Helper function to validate and process filter selections
validate_filter_selection <- function(selected_values, available_values) {
  if (is.null(selected_values) || length(selected_values) == 0 || all(selected_values == "")) {
    return(NULL)  # No filter applied
  }
  
  # Remove any values that are not in the available set
  valid_values <- selected_values[selected_values %in% available_values]
  
  if (length(valid_values) == 0) {
    return(NULL)  # No valid selections
  }
  
  return(valid_values)
}

integrate_enhanced_wildfire_system <- function() {
  message("Integrating enhanced wildfire system...")
  
  # Process wildfire data if it exists
  if (exists("wfigs_perimeters") && nrow(wfigs_perimeters) > 0) {
    tryCatch({
      wfigs_perimeters <<- process_loaded_wildfire_data(wfigs_perimeters)
      message("Enhanced wildfire data processed: ", nrow(wfigs_perimeters), " records")
    }, error = function(e) {
      message("(!!!)Warning: Could not process wildfire data: ", e$message)
    })
  } else {
    message("(!!) No wildfire data to process")
  }
  
  # Create all color palettes (this includes fire intensity and impact type palettes)
  tryCatch({
    create_color_palettes()
    message("  Color palettes created successfully")
  }, error = function(e) {
    message("(!) Warning: Could not create color palettes: ", e$message)
  })
  
  message("Enhanced wildfire system integrated")
}

# ====================================================================
# CLEAN INITIALIZATION FUNCTION WITH CACHING
# ====================================================================
initialize_system <- function() {
  message("=== Initializing Wildfire Grid Resilience System ===")
  create_output_directories()
  
  # Define cache file paths
  cache_files <- list(
    bus_info = file.path(cfg$cache_dir, "bus_info.rds"),
    branch_info = file.path(cfg$cache_dir, "branch_info.rds"),
    graph_original = file.path(cfg$cache_dir, "graph_original.rds"),
    wfigs_perimeters = file.path(cfg$cache_dir, "wfigs_perimeters.rds"),
    states_sf = file.path(cfg$cache_dir, "states_sf.rds")
  )
  
  # Check if all cache files exist
  all_cache_exists <- all(sapply(cache_files, file.exists))
  
  if (all_cache_exists) {
    message("--> Found valid cache. Loading pre-processed data...")
    tryCatch({
      bus_info <<- readRDS(cache_files$bus_info)
      buses_sf <<- bus_info # Maintain compatibility
      branch_info <<- readRDS(cache_files$branch_info)
      graph_original <<- readRDS(cache_files$graph_original)
      wfigs_perimeters <<- readRDS(cache_files$wfigs_perimeters)
      states_sf <<- readRDS(cache_files$states_sf)
      
      message("✓ All data loaded from cache successfully.")
      
    }, error = function(e) {
      message("! Error loading from cache: ", e$message)
      message("! Deleting old cache and re-processing all data.")
      unlink(cfg$cache_dir, recursive = TRUE)
      create_output_directories()
      all_cache_exists <<- FALSE # Force re-processing
    })
  }
  
  if (!all_cache_exists) {
    message("--> No valid cache found. Processing data from scratch...")
    tryCatch({
      # --- Step 1: Load all raw data ---
      load_electrical_data()
      load_wildfire_data()
      
      # --- Step 2: Create core data structures ---
      create_integrated_bus_info()
      saveRDS(bus_info, cache_files$bus_info)
      message("  ...cached bus_info.rds")
      
      create_branch_info()
      saveRDS(branch_info, cache_files$branch_info)
      message("  ...cached branch_info.rds")
      
      load_power_grid()
      saveRDS(graph_original, cache_files$graph_original)
      message("  ...cached graph_original.rds")
      
      # --- Step 3: Clean duplicate data ---
      clean_duplicate_data() # This modifies in-place, re-save after
      saveRDS(bus_info, cache_files$bus_info)
      saveRDS(branch_info, cache_files$branch_info)
      
      # --- Step 4: Create spatial data ---
      create_spatial_data()
      saveRDS(states_sf, cache_files$states_sf)
      message("  ...cached states_sf.rds")
      
      # --- Step 5: Process and enhance wildfire data (the slowest part) ---
      if (exists("wfigs_perimeters") && nrow(wfigs_perimeters) > 0) {
        wfigs_perimeters <<- process_loaded_wildfire_data(wfigs_perimeters)
        wfigs_perimeters <<- precompute_fire_impact_potential(wfigs_perimeters, bus_info)
        saveRDS(wfigs_perimeters, cache_files$wfigs_perimeters)
        message("  ...cached wfigs_perimeters.rds")
      }
      
    }, error = function(e) {
      message("\n ERROR during data processing: ", e$message)
      system_initialized <<- FALSE
      return(invisible(FALSE))
    })
  }
  initialize_healthy_state_baseline()
  
  # --- Final Steps (always run) ---
  create_color_palettes()
  system_initialized <<- TRUE
  
  if (system_initialized) {
    message("\n System initialization complete. All components loaded.")
  } else {
    message("\n (!!!) System initialization failed. Please review errors above.")
  }
  
  return(invisible(system_initialized))
}


create_color_palettes <- function() {
  tryCatch({
    # Bus type palette
    bus_pal <<- colorFactor(
      palette = c("Generator" = "#e41a1c", "Load" = "#377eb8",
                  "Gen + Load" = "#4daf4a", "Neither" = "#999999"),
      domain = c("Generator", "Load", "Gen + Load", "Neither")
    )
    
    # Fire intensity palette
    fire_intensity_pal <<- colorFactor(
      palette = c("Very Low" = "#ffffcc", "Low" = "#fed976", 
                  "Moderate" = "#feb24c", "High" = "#fd8d3c", 
                  "Extreme" = "#e31a1c", "Unknown" = "#999999"),
      domain = c("Very Low", "Low", "Moderate", "High", "Extreme", "Unknown")
    )
    
    # Fuel category color palette  
    fuel_category_pal <<- colorFactor(
      palette = c("Timber (Litter and Understory)" = "#2d5016", 
                  "Grass" = "#a1d76a", 
                  "Grass-Shrub" = "#d9ef8b",
                  "Shrub" = "#8c6239", 
                  "Hardwood" = "#543005", 
                  "Slash-Blowdown" = "#bf812d",
                  "Non-Burnable" = "#dfc27d", 
                  "Unknown" = "#999999", 
                  "Other" = "#c7eae5"),
      domain = c("Timber (Litter and Understory)", "Grass", "Grass-Shrub", "Shrub", 
                 "Hardwood", "Slash-Blowdown", "Non-Burnable", "Unknown", "Other")
    )
    
    # Primary fuel model palette (for detailed fuel types)
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      fuel_type_pal <<- colorFactor(
        palette = RColorBrewer::brewer.pal(min(12, 8), "Set3"),
        domain = NULL  # Will be set dynamically based on available data
      )
    } else {
      fuel_type_pal <<- colorFactor(
        palette = rainbow(12),
        domain = NULL
      )
    }
    
    # Landowner category color palette  
    attr_POOLandownerCategory_pal <<- colorFactor(
      palette = c("Federal" = "#2166ac", "State" = "#5aae61", "Private" = "#d73027",
                  "Local Government" = "#f46d43", "Tribal" = "#762a83", 
                  "Unknown" = "#999999", "Other" = "#c2a5cf"),
      domain = c("Federal", "State", "Private", "Local Government", "Tribal", "Unknown", "Other")
    )
    
    # Landowner kind/type color palette
    attr_POOLandownerKind_pal <<- colorFactor(
      palette = c("US Forest Service" = "#1b7837", "National Park Service" = "#5aae61",
                  "Bureau of Land Management" = "#a6dba0", "State Agency" = "#d9f0a3",
                  "Private Owner" = "#f7f7f7", "Local Government" = "#f1b6da",
                  "Tribal Land" = "#c51b7d", "Unknown" = "#999999", "Other Agency" = "#8e0152"),
      domain = c("US Forest Service", "National Park Service", "Bureau of Land Management",
                 "State Agency", "Private Owner", "Local Government", "Tribal Land", 
                 "Unknown", "Other Agency")
    )
    ggplot2::theme_set(ggplot2::theme_minimal())
    tda_dimension_colors <<- c("0" = "#2E86AB", "1" = "#E63946", "2" = "#2ca02c")
    tda_state_colors <<- c("Before" = "#4CAF50", "After" = "#E63946")
    tda_impact_colors <<- c("Fire Impact" = "#E63946", "Cascade Failures" = "#06AED5")
    
    message("Enhanced color palettes created for ggplot2 TDA workflow")
    
  }, error = function(e) {
    message("Error creating color palettes: ", e$message)
    # Create minimal fallback palettes
    bus_pal <<- colorFactor(palette = c("red", "blue", "green", "gray"), 
                            domain = c("Generator", "Load", "Gen + Load", "Neither"))
    fire_intensity_pal <<- colorFactor(palette = c("yellow", "orange", "red"), 
                                       domain = c("Low", "Moderate", "High"))
    
    # Fallback TDA colors
    tda_dimension_colors <<- c("0" = "blue", "1" = "red", "2" = "green")
    tda_state_colors <<- c("Before" = "green", "After" = "red")
    tda_impact_colors <<- c("Fire Impact" = "red", "Cascade Failures" = "blue")
  })
}
if (!exists("system_initialized") || !system_initialized) {
  system_initialized <- tryCatch({
    initialize_system()
  }, error = function(e) {
    message(" (!!!) System initialization failed with error: ", e$message)
    FALSE
  })
  
  if (!system_initialized) {
    message("(!!!) System initialization failed!")
    message("Please check your data files and try running initialize_system() manually.")
  } else {
    message("System ready for use!")
  }
} else {
  message("System already initialized.")
}