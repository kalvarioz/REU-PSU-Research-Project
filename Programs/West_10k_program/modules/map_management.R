# =================================================================================================
# modules/map_management.R
# 
# Map Management and Leaflet Functions
#
# This module contains functions for:
# - Base map initialization
# - Map updates and layer management
# - Legend creation and updates
# - Fire perimeter visualization
# - Grid network display
#
# Brandon Calvario
# =================================================================================================

# ====================================================================
# MAP MANAGEMENT
# ====================================================================
initialize_base_map <- function(output, values) {
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = states_sf, 
        layerId = ~ID, 
        fillOpacity = 0.1, 
        color = "#555", 
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 2,
          fillOpacity = 0.2,
          bringToFront = TRUE
        )
      ) %>%
      fitBounds(-125, 31, -102, 49)
    
    map <- map %>%
      addLegend("bottomright", 
                pal = bus_pal, 
                values = buses_sf$bus_type, 
                title = "Bus Type",
                layerId = "bus_legend")
    
    if (!is.null(values$initial_edges_sf) && nrow(values$initial_edges_sf) > 0) {
      map <- map %>%
        addPolylines(
          data = values$initial_edges_sf,
          group = "initial_lines",
          color = "#4169E1",
          weight = 1,
          opacity = 0.5
        )
    }
    
    if (nrow(buses_sf) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = buses_sf,
          group = "initial_buses",
          radius = 4,
          color = ~bus_pal(bus_type),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0("Bus: ", bus_i, "<br>",
                          "Type: ", bus_type, "<br>",
                          "Voltage: ", round(vm, 3), " p.u.<br>",
                          "Load: ", round(load_mw, 1), " MW<br>",
                          "Generation: ", round(total_gen, 1), " MW")
        )
    }
    map
  })
}

update_fire_display <- function() {
  # Handled in observers
}

update_bus_display <- function() {
  # Handled in observers
}

update_simulation_display <- function() {
  # Handled in observers
}

handle_map_interactions <- function() {
  # Handled in observers
}

safe_legend_management <- function() {
  safe_remove_legend <- function(map_proxy, layerId) {
    tryCatch({
      if (!is.null(layerId)) {
        map_proxy %>% removeControl(layerId = layerId)
      } else {
        map_proxy
      }
    }, error = function(e) {
      tryCatch({
        map_proxy %>% leaflet::removeControl(layerId = layerId)
      }, error = function(e2) {
        map_proxy
      })
    })
  }
  return(safe_remove_legend)
}
