library(shiny)
library(leaflet)
library(sf)
library(igraph)
library(dplyr)
library(maps)

### ---- 1. Data Loading & Preparation ----

# Load data
mpc_bus <- read.csv("mpc_bus.csv")
mpc_gen <- read.csv("mpc_gen.csv")
bus_data <- read.csv("10k_buses.csv", stringsAsFactors = FALSE)
branch_data <- read.csv("10k_branches.csv", stringsAsFactors = FALSE)
graph_original <- read_graph("10k_bus_grid.gml", format = "gml")

# Convert bus data to spatial (sf)
buses_sf <- st_as_sf(bus_data, coords = c("Longitude.1", "Latitude.1"), crs = 4326, remove = FALSE)

# Western US states context for map
states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
west_states <- c("california", "oregon", "washington", "idaho", "montana", "wyoming",
                 "nevada", "utah", "arizona", "colorado", "new mexico")
west_sf <- states_sf[states_sf$ID %in% west_states, ]

# Pre-attack metrics for each bus
bus_load_pre <- mpc_bus %>% select(bus_i, Pd,Vm)
bus_gen_pre <- mpc_gen %>%
  group_by(bus) %>%
  summarise(total_gen = sum(Pg), max_capacity = sum(Pmax)) %>%
  rename(bus_i = bus)
bus_info_pre <- bus_load_pre %>%
  left_join(bus_gen_pre, by = "bus_i") %>%
  mutate(gen_load_ratio = total_gen / (Pd + 1e-3))


### ---- 2. Wildfire Scenario Functions ----

generate_growing_fire <- function(center_lon, center_lat, steps, max_radius_km = 300) {
  perims <- vector("list", steps)
  center_point <- st_sfc(st_point(c(center_lon, center_lat)), crs = 4326) %>% st_transform(3857)
  for (i in seq_len(steps)) {
    radius <- (i / steps) * max_radius_km * 1000
    buffer <- st_buffer(center_point, radius)
    perims[[i]] <- st_transform(buffer, 4326)[[1]]
  }
  st_sfc(perims, crs = 4326)
}

simulate_attack <- function(g, buses_to_remove = NULL) {
  if (!is.null(buses_to_remove)) {
    valid_buses <- intersect(buses_to_remove, V(g)$name)
    g <- delete_vertices(g, valid_buses)
  }
  simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
}

simulate_fire_cascade <- function(graph, buses_sf, fire_polys, steps = 20) {
  graphs <- list(graph)
  fire_polys <- fire_polys[seq_len(min(steps, length(fire_polys)))]
  for (i in seq_along(fire_polys)) {
    affected_buses <- buses_sf$BusNum[st_within(buses_sf, fire_polys[i], sparse = FALSE)]
    g_new <- simulate_attack(graphs[[i]], buses_to_remove = affected_buses)
    graphs[[i + 1]] <- g_new
  }
  graphs
}

# Prepare spatial edges for mapping
prepare_edges_sf <- function(graph, bus_data) {
  bus_data <- bus_data %>%
    mutate(BusNum = as.character(BusNum))
  edges_df <- igraph::as_data_frame(graph, what = "edges") %>%
    left_join(bus_data, by = c("from" = "BusNum")) %>%
    rename(lon_from = Longitude.1, lat_from = Latitude.1) %>%
    left_join(bus_data, by = c("to" = "BusNum")) %>%
    rename(lon_to = Longitude.1, lat_to = Latitude.1) %>%
    filter(!is.na(lon_from), !is.na(lat_from), !is.na(lon_to), !is.na(lat_to))
  
  lines_list <- lapply(1:nrow(edges_df), function(i) {
    st_linestring(matrix(
      c(edges_df$lon_from[i], edges_df$lat_from[i],
        edges_df$lon_to[i], edges_df$lat_to[i]), ncol = 2, byrow = TRUE))
  })
  st_sfc(lines_list, crs = 4326)
}

### ---- 3. Generate Synthetic Wildfire Data ----

set.seed(123)
steps <- 20
center_lon <- runif(1, -122, -105)
center_lat <- runif(1, 33, 45)
synthetic_perims <- generate_growing_fire(center_lon, center_lat, steps, max_radius_km = 300)
graph_sequence <- simulate_fire_cascade(graph_original, buses_sf, synthetic_perims, steps = steps)

### ---- 4. Shiny UI ----

ui <- fluidPage(
  titlePanel("Interactive Catastrophic Fire Grid Explorer"),
  leafletOutput("map", height = "750px"),
  absolutePanel(
    top = 10, right = 10, width = 300,
    sliderInput("step", "Step", min = 1, max = length(graph_sequence),
                value = 1, step = 1, animate = animationOptions(1200)),
    verbatimTextOutput("resilience_metrics")
  )
)

### ---- 5. Shiny Server ----

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(-125, 31, -102, 49)
  })
  
  output$resilience_metrics <- renderPrint({
    current_graph <- graph_sequence[[input$step]]
    comps <- components(current_graph)
    largest_island_size <- max(comps$csize)
    num_islands <- comps$no
    in_service_buses <- as.numeric(V(current_graph)$name)
    load_served <- sum(bus_info_pre$Pd[bus_info_pre$bus_i %in% in_service_buses], na.rm = TRUE)
    load_total <- sum(bus_info_pre$Pd, na.rm = TRUE)
    cat(
      "Islands:", num_islands, "\n",
      "Largest Island Size:", largest_island_size, "\n",
      "% Load Served:", round(100 * load_served / load_total, 1), "%"
    )
  })
  
  observeEvent(input$step, {
    step <- input$step
    current_graph <- graph_sequence[[step]]
    # Join bus info for this step
    buses_step_sf <- buses_sf %>%
      filter(BusNum %in% as.numeric(V(current_graph)$name)) %>%
      left_join(bus_info_pre, by = c("BusNum" = "bus_i"))
    edges_step_sf <- prepare_edges_sf(current_graph, bus_data)
    
    # Set bus marker color based on type
    buses_step_sf <- buses_step_sf %>%
      mutate(bus_color = case_when(
        !is.na(total_gen) & total_gen > 0 ~ "green",
        Pd > 0 ~ "blue",
        TRUE ~ "gray"
      ))
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolylines(data = edges_step_sf, color = "steelblue", weight = 2, opacity = 0.7) %>%
      addCircleMarkers(
        data = buses_step_sf,
        radius = 4,
        color = ~bus_color,
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste(
          "Bus:", BusNum, "<br>",
          "Pd:", Pd, "<br>",
          "Generation:", total_gen, "<br>",
          "Gen/Load Ratio:", round(gen_load_ratio, 2),
          "Voltage: ", Vm
        )
      ) %>%
      addPolygons(
        data = st_sf(geometry = synthetic_perims[step]),
        fillColor = "orange", color = "red", fillOpacity = 0.4,
        popup = paste("Wildfire perimeter, step", step)
      )
  })
}

### ---- 6. Run App ----

shinyApp(ui, server)