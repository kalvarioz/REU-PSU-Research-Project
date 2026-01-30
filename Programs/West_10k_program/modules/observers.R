# =================================================================================================
# modules/observers.R
# 
# Observer Management Functions
#
# This module contains functions for:
# - Initialization observers
# - Data observers
# - UI update observers
# - Map observers
# - Analysis observers
#
# Brandon Calvario
# =================================================================================================

# ====================================================================
# OBSERVER MANAGEMENT
# ====================================================================
setup_initialization_observers <- function(values) {
  observe({
    tryCatch({
      if (is.null(values$initial_edges_sf)) {
        values$initial_edges_sf <- prepare_edges_sf(graph_original, bus_info)
        if (!is.null(values$initial_edges_sf)) {
          message("Initial edges prepared: ", nrow(values$initial_edges_sf), " edges")
        } else {
          message("Could not prepare initial edges")
          values$initial_edges_sf <- st_sf(
            from_bus = integer(0),
            to_bus = integer(0),
            geometry = st_sfc(crs = 4326)
          )
        }
      }
    }, error = function(e) {
      message("Could not prepare initial edges: ", e$message)
      # Create empty sf object as fallback
      values$initial_edges_sf <- st_sf(
        from_bus = integer(0),
        to_bus = integer(0), 
        geometry = st_sfc(crs = 4326)
      )
    })
  })
}

prepare_edges_sf <- function(graph = NULL, bus_coord_data = NULL) {
  # Use provided graph or default to graph_original
  target_graph <- if (!is.null(graph)) graph else {
    if (exists("graph_original")) graph_original else NULL
  }
  
  # Use provided bus data or try defaults
  coord_data <- if (!is.null(bus_coord_data)) bus_coord_data else {
    if (exists("bus_info")) bus_info else if (exists("buses_sf")) buses_sf else NULL
  }
  
  if (is.null(target_graph)) {
    message("Graph not available for edge preparation")
    return(NULL)
  }
  
  if (is.null(coord_data)) {
    message("Bus coordinate data not available for edge preparation")
    return(NULL)
  }
  
  if (ecount(target_graph) == 0 || nrow(coord_data) == 0) {
    message("Graph has no edges or no bus coordinate data available")
    return(NULL)
  }
  
  tryCatch({
    # Handle different data types (sf vs data.frame)
    if (inherits(coord_data, "sf")) {
      # If it's sf data, extract coordinates and bus_i
      coords_matrix <- st_coordinates(coord_data)
      coords <- data.frame(
        bus_i = coord_data$bus_i,
        longitude = coords_matrix[, "X"],
        latitude = coords_matrix[, "Y"]
      ) %>%
        filter(!is.na(longitude), !is.na(latitude)) %>%
        distinct(bus_i, .keep_all = TRUE)
    } else {
      # Handle different column names for coordinates
      if ("longitude" %in% names(coord_data) && "latitude" %in% names(coord_data)) {
        coords <- coord_data %>% 
          select(bus_i, longitude, latitude) %>%
          filter(!is.na(longitude), !is.na(latitude)) %>%
          distinct(bus_i, .keep_all = TRUE)
      } else if ("lon" %in% names(coord_data) && "lat" %in% names(coord_data)) {
        coords <- coord_data %>% 
          select(bus_i, lon, lat) %>%
          rename(longitude = lon, latitude = lat) %>%
          filter(!is.na(longitude), !is.na(latitude)) %>%
          distinct(bus_i, .keep_all = TRUE)
      } else {
        message("Could not find coordinate columns in bus data")
        return(NULL)
      }
    }
    
    if (nrow(coords) == 0) {
      message("No valid coordinates found")
      return(NULL)
    }
    
    edges_df <- igraph::as_data_frame(target_graph, what = "edges") %>%
      transmute(
        from_bus = as.integer(from), 
        to_bus = as.integer(to)
      ) %>%
      distinct(from_bus, to_bus, .keep_all = TRUE) %>%
      left_join(coords, by = c("from_bus" = "bus_i"), relationship = "many-to-one") %>%
      rename(lon_from = longitude, lat_from = latitude) %>%
      left_join(coords, by = c("to_bus" = "bus_i"), relationship = "many-to-one") %>%
      rename(lon_to = longitude, lat_to = latitude) %>%
      filter(!is.na(lon_from), !is.na(lat_from), !is.na(lon_to), !is.na(lat_to))
    
    if (nrow(edges_df) == 0) {
      message("No edges with valid coordinates found")
      return(NULL)
    }
    
    edge_lines <- lapply(1:nrow(edges_df), function(i) {
      st_linestring(matrix(c(edges_df$lon_from[i], edges_df$lat_from[i], 
                             edges_df$lon_to[i], edges_df$lat_to[i]), 
                           ncol = 2, byrow = TRUE))
    })
    
    edges_sf <- st_sf(
      from_bus = edges_df$from_bus,
      to_bus = edges_df$to_bus,
      geometry = st_sfc(edge_lines, crs = 4326)
    )
    
    message("Successfully prepared ", nrow(edges_sf), " edge connections")
    return(edges_sf)
    
  }, error = function(e) {
    message("Error preparing edges: ", e$message)
    return(NULL)
  })
}

setup_data_observers <- function(input, values, session) {
  # Update analysis step slider when cascade completes
  observe({
    if (!is.null(values$cascade_results)) {
      max_steps <- length(values$cascade_results$graphs) - 1
      if (max_steps > 0) {
        updateSliderInput(session, "tda_analysis_step", 
                          min = 1, max = max_steps, 
                          value = min(ifelse(is.null(input$tda_analysis_step), 1, input$tda_analysis_step), max_steps))
      }
    }
  })
}

setup_ui_update_observers <- function(input, output, session) {
  # Fire color mode observer
  observeEvent(input$fire_color_mode, {
    if (is.null(input$fire_color_mode)) return()
    # Trigger map update when color mode changes
    # (Map update logic handled in map management)
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
}

setup_map_observers <- function(input, values, selected_fire, selected_state, session) {
  # Map click handler
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id) && click$id %in% cfg$western_states) {
      # Use the selected_state parameter passed to this function
      if (!is.null(selected_state)) {
        selected_state(click$id)
      }
      
      # Update the UI input
      if (!is.null(session)) {
        updateSelectInput(session, "state_select", selected = click$id)
        
        # Reset downstream selections
        updateSelectInput(session, "fire_intensity_select", selected = "")
        updateSelectInput(session, "fire_event", selected = "")
      }
    }
  })
  observe({
    req(values$system_ready)
    
    # Only proceed if fires should be shown
    if (is.null(input$show_fires) || !input$show_fires) {
      # If fires shouldn't be shown, clear fire groups
      leafletProxy("map") %>% 
        clearGroup("fire_perimeters") %>%
        clearGroup("fire_centers") %>%
        clearGroup("impact_zones")
      return()
    }
    
    # Only proceed if we have a selected fire
    fire_data <- tryCatch({
      selected_fire()
    }, error = function(e) {
      message("Error accessing selected fire in observer: ", e$message)
      return(NULL)
    })
    
    if (is.null(fire_data) || !inherits(fire_data, "sf") || nrow(fire_data) == 0) {
      # Clear fire groups if no data
      leafletProxy("map") %>% 
        clearGroup("fire_perimeters") %>%
        clearGroup("fire_centers") %>%
        clearGroup("impact_zones")
      return()
    }
    
    tryCatch({
      proxy <- leafletProxy("map") %>% 
        clearGroup("fire_perimeters") %>%
        clearGroup("fire_centers") %>%
        clearGroup("impact_zones")
      
      # Remove existing fire legends
      tryCatch({
        proxy <- proxy %>% 
          removeControl(layerId = "fire_legend") %>%
          removeControl(layerId = "fuel_legend") %>%
          removeControl(layerId = "landowner_legend")
      }, error = function(e) {
        # Ignore legend removal errors
      })
      
      # Validate and fix geometries
      if (any(!st_is_valid(fire_data))) {
        fire_data <- st_make_valid(fire_data)
      }
      
      unique_fires <- length(unique(fire_data$attr_IncidentName))
      is_compound_event <- unique_fires > 1
      
      # Get color mode safely
      color_mode <- tryCatch({
        if (!is.null(input$fire_color_mode)) {
          input$fire_color_mode
        } else {
          "intensity"
        }
      }, error = function(e) {
        "intensity"
      })
      
      fire_data <- fire_data %>%
        mutate(
          # Ensure required columns exist with defaults
          attr_IncidentName = ifelse(is.na(attr_IncidentName) | attr_IncidentName == "", 
                                     "Unknown Fire", as.character(attr_IncidentName)),
          fire_intensity = ifelse(is.na(fire_intensity) | fire_intensity == "", 
                                  "Unknown", as.character(fire_intensity)),
          fire_acres = ifelse(is.na(fire_acres), 0, as.numeric(fire_acres)),
          
          # Create popup text using case_when for proper vectorization
          popup_text = case_when(
            is_compound_event ~ paste0("Compound Fire Event<br>",
                                       "Fire: ", attr_IncidentName, "<br>",
                                       "Event fires: ", unique_fires, "<br>",
                                       "This fire area: ", round(fire_acres), " acres<br>",
                                       "Intensity: ", fire_intensity),
            TRUE ~ paste0("Fire: ", attr_IncidentName, "<br>",
                          "Intensity: ", fire_intensity, "<br>",
                          "Area: ", round(fire_acres), " acres")
          )
        )
      
      message("Adding fire data to map: ", nrow(fire_data), " polygons")
      message("Color mode: ", color_mode)
      message("Is compound event: ", is_compound_event)
      
      # Apply appropriate coloring with enhanced popup
      if (color_mode == "attr_PrimaryFuelModel" && exists("fuel_category_pal")) {
        tryCatch({
          if (!"fuel_category" %in% names(fire_data)) {
            message("fuel_category column not found, using default coloring")
            color_mode <- "intensity"
          } else {
            proxy <- proxy %>%
              addPolygons(
                data = fire_data,
                group = "fire_perimeters",
                color = ~fuel_category_pal(fuel_category), 
                weight = if (is_compound_event) 3 else 2,
                fillOpacity = if (is_compound_event) 0.5 else 0.4,
                popup = ~popup_text
              ) %>%
              addLegend("topleft", 
                        pal = fuel_category_pal, 
                        values = fire_data$fuel_category, 
                        title = if (is_compound_event) "Fuel Category (Compound)" else "Fuel Category",
                        layerId = "fuel_legend")
            message("Added fuel category polygons to map")
            return()
          }
        }, error = function(e) {
          message("Error adding fuel category polygons: ", e$message)
          color_mode <- "intensity"
        })
      }
      
      if (color_mode == "attr_POOLandownerCategory" && exists("attr_POOLandownerCategory_pal")) {
        tryCatch({
          if (!"landowner_category" %in% names(fire_data)) {
            message("landowner_category column not found, using default coloring")
            color_mode <- "intensity"
          } else {
            proxy <- proxy %>%
              addPolygons(
                data = fire_data,
                group = "fire_perimeters", 
                color = ~attr_POOLandownerCategory_pal(landowner_category),
                weight = if (is_compound_event) 3 else 2,
                fillOpacity = if (is_compound_event) 0.5 else 0.4,
                popup = ~popup_text
              ) %>%
              addLegend("topleft",
                        pal = attr_POOLandownerCategory_pal,
                        values = fire_data$landowner_category,
                        title = if (is_compound_event) "Landowner (Compound)" else "Landowner Category", 
                        layerId = "landowner_legend")
            message("Added landowner category polygons to map")
            return()
          }
        }, error = function(e) {
          message("Error adding landowner category polygons: ", e$message)
          color_mode <- "intensity"
        })
      }
      
      # Default intensity coloring with compound event styling
      if (exists("fire_intensity_pal")) {
        tryCatch({
          proxy <- proxy %>%
            addPolygons(
              data = fire_data,
              group = "fire_perimeters",
              color = ~fire_intensity_pal(fire_intensity), 
              weight = if (is_compound_event) 3 else 2,
              fillOpacity = if (is_compound_event) 0.5 else 0.4,
              popup = ~popup_text
            ) %>%
            addLegend("topleft", 
                      pal = fire_intensity_pal, 
                      values = fire_data$fire_intensity, 
                      title = if (is_compound_event) "Fire Intensity (Compound)" else "Fire Intensity",
                      layerId = "fire_legend")
          message("Added fire intensity polygons to map")
        }, error = function(e) {
          message("Error adding intensity polygons: ", e$message)
        })
      } else {
        message("Warning: fire_intensity_pal palette not found")
      }
      
    }, error = function(e) {
      message("Error in fire display observer: ", e$message)
    })
  })
  # Map layer visibility control
  observe({
    proxy <- leafletProxy("map")
    groups_to_show <- c()
    groups_to_hide <- c()
    
    if (!is.null(input$show_buses) && input$show_buses) groups_to_show <- c(groups_to_show, "initial_buses")
    else groups_to_hide <- c(groups_to_hide, "initial_buses")
    
    if (!is.null(input$show_lines) && input$show_lines) groups_to_show <- c(groups_to_show, "initial_lines")
    else groups_to_hide <- c(groups_to_hide, "initial_lines")
    
    if (!is.null(input$show_fires) && input$show_fires) groups_to_show <- c(groups_to_show, "fire_perimeters")
    else groups_to_hide <- c(groups_to_hide, "fire_perimeters")
    
    if (!is.null(input$show_fire_centers) && input$show_fire_centers) groups_to_show <- c(groups_to_show, "fire_centers")
    else groups_to_hide <- c(groups_to_hide, "fire_centers")
    
    if (!is.null(input$show_impact_zones) && input$show_impact_zones) groups_to_show <- c(groups_to_show, "impact_zones")
    else groups_to_hide <- c(groups_to_hide, "impact_zones")
    
    for (group in groups_to_show) proxy %>% showGroup(group)
    for (group in groups_to_hide) proxy %>% hideGroup(group)
  })
}


setup_analysis_observers <- function(input, values, session = NULL) {
  observe({
    req(values$cascade_results, input$step)
    
    step <- input$step
    cascade_result <- values$cascade_results
    
    # Get current graph state and failed components
    g_cur <- cascade_result$graphs[[step + 1]] # Graphs list is 1-indexed by step+1
    active_buses <- as.numeric(V(g_cur)$name)
    buses_lost_so_far <- unique(unlist(cascade_result$buses_lost_per_step[1:step]))
    
    # Prepare visualization data
    buses_active_sf <- buses_sf %>% filter(bus_i %in% active_buses)
    
    fire_points <- if (step <= length(cascade_result$fire_points_list)) {
      points_data <- cascade_result$fire_points_list[[step]]
      
      # Ensure fire points data has the expected structure
      if (!is.null(points_data) && nrow(points_data) > 0) {
        # Check if compound fire information is available
        if ("is_compound_impact" %in% names(points_data)) {
          message("Step ", step, ": Processing fire points with compound event information")
          points_data
        } else {
          # Add compound event information if missing
          points_data %>%
            mutate(
              is_compound_impact = FALSE,
              affecting_fire_count = 1,
              affecting_fire_names = if("attr_IncidentName" %in% names(.)) attr_IncidentName else "Unknown"
            )
        }
      } else {
        buses_sf[0, ] %>%
          mutate(
            impact_type = character(0),
            fire_step = integer(0),
            is_compound_impact = logical(0),
            affecting_fire_count = integer(0),
            affecting_fire_names = character(0)
          )
      }
    } else {
      buses_sf[0, ] %>%
        mutate(
          impact_type = character(0),
          fire_step = integer(0),
          is_compound_impact = logical(0),
          affecting_fire_count = integer(0),
          affecting_fire_names = character(0)
        )
    }
    
    # Get all buses that have failed up to this point
    buses_failed_sf <- buses_sf %>% filter(bus_i %in% buses_lost_so_far)
    
    edges_active_sf <- prepare_edges_sf(g_cur, bus_info)
    
    # Find edges connected to at least one failed bus
    failed_edges_df <- igraph::as_data_frame(graph_original, what = "edges") %>%
      filter(from %in% buses_lost_so_far | to %in% buses_lost_so_far)
    failed_edges_graph <- graph_from_data_frame(failed_edges_df, directed = FALSE)
    edges_failed_sf <- prepare_edges_sf(failed_edges_graph, bus_info)
    
    #Create appropriate fire icons based on compound status
    if (nrow(fire_points) > 0 && any(fire_points$is_compound_impact, na.rm = TRUE)) {
      # Compound fire icon (larger, different color)
      fireIcon <- makeIcon(
        iconUrl = "https://img.icons8.com/plasticine/100/ff6b35/fire-element.png",
        iconWidth = 35, iconHeight = 35,
        iconAnchorX = 17, iconAnchorY = 17
      )
      message("Using compound fire icons for step ", step)
    } else {
      # Standard fire icon
      fireIcon <- makeIcon(
        iconUrl = "https://img.icons8.com/plasticine/100/000000/fire-element.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 15, iconAnchorY = 15
      )
    }
    
    # Update map
    proxy <- leafletProxy("map") %>% 
      clearGroup("simulation") %>%
      clearGroup("failed_elements") %>%
      hideGroup("initial_buses") %>%
      hideGroup("initial_lines")
    
    # Add FAILED branches (in red)
    if (input$show_lines && !is.null(edges_failed_sf) && nrow(edges_failed_sf) > 0) {
      proxy <- proxy %>%
        addPolylines(
          data = edges_failed_sf, 
          group = "failed_elements",
          color = "#E74C3C",
          weight = 2, 
          opacity = 0.8
        )
    }
    
    # Add ACTIVE branches
    if (input$show_lines && !is.null(edges_active_sf) && nrow(edges_active_sf) > 0) {
      proxy <- proxy %>%
        addPolylines(
          data = edges_active_sf, 
          group = "simulation",
          color = "#0066CC", 
          weight = 2, 
          opacity = 0.7
        )
    }
    
    # Add FAILED buses (as gray 'X's)
    if (input$show_buses && nrow(buses_failed_sf) > 0) {
      proxy <- proxy %>%
        addMarkers(
          data = buses_failed_sf,
          group = "failed_elements",
          icon = makeIcon(iconUrl = "https://img.icons8.com/ios-filled/50/a0a0a0/delete-sign.png", 
                          iconWidth=12, iconHeight=12),
          popup = ~paste0("Bus: ", bus_i, "  Status: FAILED")
        )
    }
    
    # Add ACTIVE buses
    if (input$show_buses && nrow(buses_active_sf) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = buses_active_sf,
          group = "simulation",
          radius = 5, 
          color = ~bus_pal(bus_type), 
          fillOpacity = 0.8,
          stroke = TRUE, weight = 1,
          popup = ~paste0("Bus: ", bus_i, "<br>Type: ", bus_type, "<br>Status: Active")
        )
    }
    
    if (nrow(fire_points) > 0) {
      
      message("Adding ", nrow(fire_points), " fire icons for step ", step)
      
      # Create enhanced popup text for fire-affected buses
      fire_points_with_popup <- fire_points %>%
        mutate(
          popup_text = case_when(
            is_compound_impact ~ paste0(
              "Bus: ", bus_i, 
              "  Impact: Compound Fire Event",
              "  Step: ", fire_step,
              "  Fire Count: ", affecting_fire_count,
              "  Fires: ", affecting_fire_names,
              "  Impact Type: ", tools::toTitleCase(impact_type)
            ),
            TRUE ~ paste0(
              "Bus: ", bus_i, 
              "<br>Impact: Fire",
              "<br>Step: ", fire_step,
              "<br>Impact Type: ", tools::toTitleCase(impact_type)
            )
          )
        )
      
      proxy <- proxy %>%
        addMarkers(
          data = fire_points_with_popup,
          group = "failed_elements",
          icon = fireIcon,
          popup = ~popup_text
        )
      
      # Log fire icon placement for debugging
      compound_count <- sum(fire_points$is_compound_impact, na.rm = TRUE)
      if (compound_count > 0) {
        message("Placed ", compound_count, " compound fire icons and ", 
                nrow(fire_points) - compound_count, " standard fire icons")
      } else {
        message("Placed ", nrow(fire_points), " standard fire icons")
      }
    } else {
      message("No fire points to display for step ", step)
    }
  })
}