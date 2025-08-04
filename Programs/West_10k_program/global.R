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
library(R6)  # For observer pattern implementation

setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")
monitor_connections <- function() {
  tryCatch({
    all_cons <- showConnections(all = TRUE)
    total_connections <- nrow(all_cons)
    
    # Count different types of connections
    if (total_connections > 0) {
      connection_types <- table(all_cons[, "class"])
      
      # Log if there are many connections
      if (total_connections > 10) {
        message("High connection count detected: ", total_connections)
        message("Connection types: ", paste(names(connection_types), "=", connection_types, collapse = ", "))
      }
    }
    
    return(total_connections)
  }, error = function(e) {
    message("Error monitoring connections: ", e$message)
    return(0)
  })
}

# FIX: Comprehensive parallel cleanup function to prevent connection leaks
cleanup_parallel_resources <- function(force_cleanup = FALSE) {
  message("=== COMPREHENSIVE PARALLEL CLEANUP ===")
  
  tryCatch({
    # 1. Stop all future workers first
    if (requireNamespace("future", quietly = TRUE)) {
      current_plan <- future::plan()
      if (inherits(current_plan, "multisession") || inherits(current_plan, "multicore")) {
        message("  Stopping future workers...")
        future::plan(future::sequential)
        Sys.sleep(0.5)  # Give time for workers to shut down
      }
    }
    
    # 2. Stop doParallel clusters
    if (requireNamespace("doParallel", quietly = TRUE) && requireNamespace("foreach", quietly = TRUE)) {
      if (foreach::getDoParRegistered()) {
        message("  Stopping doParallel workers...")
        foreach::registerDoSEQ()
      }
    }
    
    # 3. CRITICAL FIX: Close lingering connections more safely
    all_cons <- showConnections(all = TRUE)
    if (nrow(all_cons) > 3) {  # Keep stdin, stdout, stderr
      
      problematic_cons <- all_cons[all_cons[, "class"] %in% c("textConnection", "sockconn", "file"), , drop = FALSE]
      
      if (nrow(problematic_cons) > 0) {
        message("  Closing ", nrow(problematic_cons), " problematic connections...")
        
        for (i in 1:nrow(problematic_cons)) {
          con_num <- as.integer(rownames(problematic_cons)[i])
          con_desc <- problematic_cons[i, "description"]
          
          if (grepl("stdin|stdout|stderr", con_desc, ignore.case = TRUE)) next
          
          tryCatch({
            close(con_num)
            message("    Closed connection ", con_num, ": ", con_desc)
          }, error = function(e) {
            tryCatch({
              con_obj <- getConnection(con_num)
              close(con_obj)
            }, error = function(e2) {
              message("    Could not close connection ", con_num, ": ", e2$message)
            })
          })
        }
      }
    }
    
    # 4. Reset data.table threads
    if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::setDTthreads(1)
      message("  Reset data.table to single thread")
    }
    
    # 5. Force garbage collection
    gc(verbose = FALSE)
    
    # 6. Final connection count
    final_cons <- nrow(showConnections(all = TRUE))
    message("  Final connection count: ", final_cons)
    
    message("✓ Comprehensive parallel cleanup complete")
    
  }, error = function(e) {
    message("Error during comprehensive cleanup: ", e$message)
  })
  
  return(invisible(TRUE))
}

# FIX: Simplified, safer parallel setup for Shiny
setup_parallel_processing <- function(max_workers = 2) {
  
  cleanup_parallel_resources(force_cleanup = TRUE)
  total_cores <- parallel::detectCores()
  n_cores <- min(max_workers, max(1, total_cores - 2))
  
  message("=== SIMPLE PARALLEL SETUP ===")
  message("Total cores: ", total_cores)
  message("Using cores: ", n_cores)
  
  if (n_cores <= 1) {
    message("Using sequential processing (safest option)")
    future::plan(future::sequential)
    return(1)
  }
  
  tryCatch({
    future::plan(future::multisession, workers = n_cores)
    options(future.globals.maxSize = 500 * 1024^2)
    options(future.rng.onMisuse = "ignore")
    
    message("✓ Simple parallel setup complete: ", n_cores, " workers")
    return(n_cores)
    
  }, error = function(e) {
    message("Error in parallel setup, falling back to sequential: ", e$message)
    future::plan(future::sequential)
    return(1)
  })
}

# FIX: Safe parallel execution wrapper
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
  outputs_attacked_dir = "outputsAttacked/", # <-- ADD THIS LINE
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
  memory_limit_gb = 4  
)


create_output_directories <- function() {
  dirs_to_create <- c(
    cfg$outputs_dir,
    cfg$outputs_attacked_dir, # <-- ADD THIS LINE
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
  
  message("Loading electrical grid data...")
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
  mpc_bus <<- fread(files_to_check$mpc_bus) %>%
    select(bus_i, type, pd = Pd, vm = Vm, va = Va, baseKV, zone, vmax = Vmax, vmin = Vmin) %>%
    distinct(bus_i, .keep_all = TRUE)  # Remove duplicates immediately
  bus_data <<- fread(files_to_check$bus_data) %>%
    rename(bus_i = BusNum, bus_name = BusName, longitude = Longitude, latitude = Latitude) %>%
    mutate(bus_i = as.integer(bus_i)) %>%
    distinct(bus_i, .keep_all = TRUE)  # Remove duplicates immediately
  mpc_branch <<- fread(files_to_check$mpc_branch) %>%
    select(from_bus = fbus, to_bus = tbus, r, x, b, rateA, rateB, rateC) %>%
    mutate(b1 = pmin(from_bus, to_bus), b2 = pmax(from_bus, to_bus)) %>%
    distinct(b1, b2, .keep_all = TRUE)  # Remove duplicate connections
  branch_data <<- fread(files_to_check$branch_data) %>%
    rename(from_bus = BusNum, to_bus = BusNum.1, line_r = LineR, line_x = LineX) %>%
    mutate(
      from_bus = as.integer(from_bus),
      to_bus = as.integer(to_bus),
      b1 = pmin(from_bus, to_bus), 
      b2 = pmax(from_bus, to_bus)
    ) %>%
    distinct(b1, b2, .keep_all = TRUE)  # Remove duplicate connections
  gen_data <<- fread(files_to_check$mpc_gen) %>%
    rename(bus_i = bus, Pg = Pg, Qg = Qg, Qmax = Qmax, Qmin = Qmin, 
           Vg = Vg, mBase = mBase, status = status, Pmax = Pmax, Pmin = Pmin)
  load_data <<- fread(files_to_check$load_data) %>%
    rename(bus_i = BusNum, load_mw = LoadSMW, load_mvar = LoadSMVR)
  message("Electrical data loaded successfully")
  
  message("mpc_bus: ", nrow(mpc_bus), " buses")
  message("bus_data: ", nrow(bus_data), " buses")
  message("mpc_branch: ", nrow(mpc_branch), " branches")
  
  message("branch_data: ", nrow(branch_data), " branches")
  
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
  cat("   mpc_bus rows:", nrow(mpc_bus), "\n")
  cat("   bus_data rows:", nrow(bus_data), "\n") 
  cat("   mpc_branch rows:", nrow(mpc_branch), "\n")
  cat("   branch_data rows:", nrow(branch_data), "\n\n")
  
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
  message("Creating integrated bus information...")
  
  # Create bus info data
  bus_info_temp <- mpc_bus %>%
    left_join(bus_data, by = "bus_i") %>%
    left_join(
      gen_data %>% group_by(bus_i) %>% summarise(total_gen = sum(Pg, na.rm = TRUE), .groups = 'drop'),
      by = "bus_i"
    ) %>%
    left_join(load_data, by = "bus_i") %>%
    mutate(
      total_gen = ifelse(is.na(total_gen), 0, total_gen),
      load_mw = ifelse(is.na(load_mw), 0, load_mw),
      bus_type = case_when(
        total_gen > 0 & load_mw > 0 ~ "Gen + Load",
        total_gen > 0 ~ "Generator",
        load_mw > 0 ~ "Load",
        TRUE ~ "Neither"
      )
    )
  
  message("  Raw bus data: ", nrow(bus_info_temp), " rows")
  
  # Check for coordinate columns
  if (!"longitude" %in% names(bus_info_temp) || !"latitude" %in% names(bus_info_temp)) {
    stop("Missing longitude or latitude columns in bus data")
  }
  
  # Filter for valid coordinates
  bus_info_with_coords <- bus_info_temp %>%
    filter(!is.na(longitude), !is.na(latitude), 
           abs(longitude) > 0, abs(latitude) > 0,
           abs(longitude) <= 180, abs(latitude) <= 90)
  
  message("  Buses with valid coordinates: ", nrow(bus_info_with_coords))
  
  # Create sf object with robust error handling
  if (nrow(bus_info_with_coords) > 0) {
    tryCatch({
      bus_info <<- st_as_sf(bus_info_with_coords, 
                            coords = c("longitude", "latitude"), 
                            crs = 4326, 
                            remove = FALSE)
      
      # Verify it's actually an sf object
      if (!inherits(bus_info, "sf")) {
        stop("Failed to create sf object - st_as_sf returned non-sf object")
      }
      
      message("  ✓ Successfully created sf object: ", nrow(bus_info), " buses")
      
    }, error = function(e) {
      message("  ✗ Error creating sf object: ", e$message)
      message("  Creating fallback sf object...")
      
      # Create a minimal sf object as fallback
      empty_sf <- st_sf(
        bus_i = integer(0),
        total_gen = numeric(0),
        load_mw = numeric(0), 
        bus_type = character(0),
        geometry = st_sfc(crs = 4326)
      )
      bus_info <<- empty_sf
      
      warning("Bus spatial data creation failed. Using empty sf object.")
    })
    
  } else {
    message("  ⚠ No buses with valid coordinates found")
    message("  Creating empty sf object...")
    
    # Create empty sf object with correct structure
    empty_sf <- st_sf(
      bus_i = integer(0),
      type = integer(0),
      pd = numeric(0),
      vm = numeric(0),
      va = numeric(0),
      baseKV = numeric(0),
      zone = integer(0),
      vmax = numeric(0),
      vmin = numeric(0),
      bus_name = character(0),
      longitude = numeric(0),
      latitude = numeric(0),
      total_gen = numeric(0),
      load_mw = numeric(0),
      load_mvar = numeric(0),
      bus_type = character(0),
      geometry = st_sfc(crs = 4326)
    )
    bus_info <<- empty_sf
  }
  
  # Final validation
  if (!inherits(bus_info, "sf")) {
    stop("Critical error: bus_info is not an sf object after creation")
  }
  
  # Create buses_sf for compatibility (can be deprecated later)
  buses_sf <<- bus_info 
  
  message("✓ Integrated bus_info created: ", nrow(bus_info), " buses")
  message("  Object class: ", paste(class(bus_info), collapse = ", "))
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
    
    filter(!is.na(r), !is.na(x)) %>%  # Only keep branches with valid electrical parameters
    distinct(b1, b2, .keep_all = TRUE)  # Final deduplication
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
    "Timber Litter and Understory)" = "Timber/Understory", # Handle typo
    "Brush (2 feet)" = "Brush"
  )
  # ---------- SECONDARY FUEL MODEL -----------------
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
    data$attr_SecondaryFuelModel    <- "Unknown"
    data$fuel_category_secondary    <- "Unknown"
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
  
  # Create mapping for detailed landowner types (from your images)
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
        # Your existing standardized category names
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
        # Enhanced simple categories (from your images: Federal, Other, Private, Blanks)
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
  # Complete mapping including truncated shapefile names
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
    
    # NEW FILTER COLUMNS
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
        message("    Mapped ", possible_name, " -> ", standard_name)
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
  
  # Process new filter columns with proper handling
  data <- process_fuel_and_landowner_data(data)
  
  return(data)
}

add_date_and_step_info <- function(data) {
  message("    Processing date and step information...")
  
  date_columns <- c("poly_PolygonDateTime", "poly_Polyg", "poly_CreateDate", "poly_Creat", "CreateDate")
  date_col <- NULL
  
  for (col in date_columns) {
    if (col %in% names(data) && !all(is.na(data[[col]]))) {
      date_col <- col
      break
    }
  }
  
  if (!is.null(date_col)) {
    message("      Using date column: ", date_col)
    
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
          
          # Final fallback - FIXED: No return() statement
          if (all(is.na(parsed_dates))) {
            rep(Sys.time(), length(date_strings))
          } else {
            parsed_dates
          }
          
        }, error = function(e) {
          message("        Date parsing error: ", e$message)
          # FIXED: Use rep() instead of n()
          rep(Sys.time(), nrow(data))
        }),
        
        date_only = as.Date(date_time)
      )
    
    # Create steps based on incident progression
    if ("attr_IncidentName" %in% names(data)) {
      data <- data %>%
        group_by(attr_IncidentName) %>%
        arrange(date_time) %>%
        mutate(
          step = tryCatch({
            valid_dates <- date_only[!is.na(date_only)]
            if (length(valid_dates) == 0) {
              1L
            } else {
              unique_dates <- sort(unique(valid_dates))
              step_values <- match(date_only, unique_dates)
              step_values[is.na(step_values)] <- 1L
              as.integer(step_values)
            }
          }, error = function(e) {
            message("        Step creation error: ", e$message)
            # FIXED: Use rep() with group size
            rep(1L, n())
          })
        ) %>%
        ungroup()
    } else {
      data$step <- 1L
    }
    
  } else {
    message("      No valid date column found, using defaults")
    data$date_time <- Sys.time()
    data$date_only <- Sys.Date()
    data$step <- 1L
  }
  
  # Final validation
  data$step[is.na(data$step)] <- 1L
  data$step[data$step < 1] <- 1L
  
  message("      Date processing complete. Steps range: ", min(data$step, na.rm = TRUE), 
          " to ", max(data$step, na.rm = TRUE))
  
  return(data)
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
      message("    Found ", sum(valid_coords), " fires with initial coordinates")
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
  message("  Processing wildfire data...")
  
  data <- standardize_wildfire_columns(data)
  
  # Filter by western states
  if ("attr_POOState" %in% names(data)) {
    before_count <- nrow(data)
    data <- data %>%
      filter(tolower(attr_POOState) %in% cfg$western_states | is.na(attr_POOState))
    after_count <- nrow(data)
    message("    Filtered by western states: ", before_count, " -> ", after_count)
  }
  
  # Process in order
  data <- add_date_and_step_info(data)
  data <- classify_fire_intensity(data)
  data <- create_fire_center_points(data)
  
  # Fix any geometry issues
  if (inherits(data, "sf") && any(!st_is_valid(data))) {
    message("    Fixing invalid geometries...")
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
  
  # Check if perseus_V3.R functions are available
  if (!exists("load_net_power_matrix")) {
    stop("perseus_V3.R functions not loaded. Source perseus_V3.R first.")
  }
  
  csv_file <- "databases/net_power_difference_normalizedTEST.csv"
  
  if (!file.exists(csv_file)) {
    message("⚠️  CSV file not found: ", csv_file)
    message("    You need to create: ", csv_file)
    return(list(success = FALSE, error = "CSV file missing"))
  }
  
  tryCatch({
    # Use the existing perseus_V3.R function directly
    healthy_matrix <- load_net_power_matrix(csv_file)
    
    # Run complete Perseus analysis using existing function
    healthy_dir <- file.path(cfg$outputs_dir, "healthy_state")
    healthy_results <- run_perseus_analysis(healthy_matrix, healthy_dir)
    
    if (healthy_results$success) {
      # Store globally for comparisons
      assign("healthy_state_matrix", healthy_matrix, envir = .GlobalEnv)
      assign("healthy_state_persistence", healthy_results$persistence_data, envir = .GlobalEnv)
      
      message("✓ Healthy state baseline established")
      message("  Matrix size: ", nrow(healthy_matrix), "×", ncol(healthy_matrix))
      message("  Features found: ", nrow(healthy_results$persistence_data))
      
      return(list(
        success = TRUE,
        matrix = healthy_matrix,
        persistence_data = healthy_results$persistence_data,
        analysis_dir = healthy_dir
      ))
    } else {
      return(list(success = FALSE, error = healthy_results$error))
    }
    
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
  
  # NEW: Show fuel type and landowner summaries
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



# Update the existing create_color_palettes function
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
      message("  Enhanced wildfire data processed: ", nrow(wfigs_perimeters), " records")
    }, error = function(e) {
      message("  Warning: Could not process wildfire data: ", e$message)
    })
  } else {
    message("  No wildfire data to process")
  }
  
  # Create all color palettes (this includes fire intensity and impact type palettes)
  tryCatch({
    create_color_palettes()
    message("  Color palettes created successfully")
  }, error = function(e) {
    message("  Warning: Could not create color palettes: ", e$message)
  })
  
  message("✓ Enhanced wildfire system integrated")
}

# ====================================================================
# CLEAN INITIALIZATION FUNCTION
# ====================================================================
initialize_system <- function() {
  message("=== Initializing Wildfire Grid Resilience System ===")
  create_output_directories()
  
  tryCatch({
    # --- Step 1: Load all raw data ---
    load_electrical_data()
    load_wildfire_data()
    
    # --- Step 2: Create core data structures ---
    create_integrated_bus_info()
    create_branch_info()
    clean_duplicate_data()
    load_power_grid()
    
    # --- Step 3: Create spatial data (including states_sf) ---
    create_spatial_data()
    
    # --- Step 4: Process and enhance data ---
    if (exists("wfigs_perimeters") && nrow(wfigs_perimeters) > 0) {
      wfigs_perimeters <<- process_loaded_wildfire_data(wfigs_perimeters)
      wfigs_perimeters <<- precompute_fire_impact_potential(wfigs_perimeters, bus_info)
    }
    
    # --- Step 5: Create UI elements ---
    create_color_palettes()
    
    # --- Step 6: Set readiness flag LAST ---
    system_ready <<- TRUE
    
  }, error = function(e) {
    message("\n❌ ERROR during initialization: ", e$message)
    system_ready <<- FALSE
  })
  
  if (exists("system_ready") && system_ready) {
    message("\n✅ System initialization complete. All components loaded.")
  } else {
    message("\n❌ System initialization failed. Please review errors above.")
  }
  
  return(invisible(exists("system_ready") && system_ready))
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
    
    message("✓ Enhanced color palettes created for new filtering system")
    
  }, error = function(e) {
    message("Error creating color palettes: ", e$message)
    # Create minimal fallback palettes
    bus_pal <<- colorFactor(palette = c("red", "blue", "green", "gray"), 
                            domain = c("Generator", "Load", "Gen + Load", "Neither"))
    fire_intensity_pal <<- colorFactor(palette = c("yellow", "orange", "red"), 
                                       domain = c("Low", "Moderate", "High"))
  })
}
# FIXED: Make sure system_initialized is properly set
if (!exists("system_initialized") || !system_initialized) {
  system_initialized <- tryCatch({
    initialize_system()
  }, error = function(e) {
    message("❌ System initialization failed with error: ", e$message)
    FALSE
  })
  
  if (!system_initialized) {
    message("❌ System initialization failed!")
    message("Please check your data files and try running initialize_system() manually.")
  } else {
    message("✅ System ready for use!")
  }
} else {
  message("System already initialized.")
}
