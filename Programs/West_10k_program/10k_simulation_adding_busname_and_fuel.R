library(shiny)
library(leaflet)
library(sf)
library(igraph)
library(dplyr)
library(maps)
library(ggplot2)

# Load MATPOWER-derived data
mpc_bus   <- read.csv("mpc_bus.csv",      stringsAsFactors = FALSE)
mpc_gen   <- read.csv("mpc_gen.csv",      stringsAsFactors = FALSE)
bus_names <- read.csv("mpc_bus_name.csv", stringsAsFactors = FALSE)
fuel_types<- read.csv("mpc_genfuel.csv",  stringsAsFactors = FALSE)

# Attach bus names and fuel types
mpc_bus$bus_name     <- bus_names$bus_name
mpc_gen$fuel_type    <- fuel_types$fuel_type

# Load geospatial data for plotting
bus_data    <- read.csv("10k_buses.csv",    stringsAsFactors = FALSE)
branch_data <- read.csv("10k_branches.csv", stringsAsFactors = FALSE)
graph_original <- read_graph("10k_bus_grid.gml", format = "gml")
buses_sf <- st_as_sf(bus_data, coords = c("Longitude.1", "Latitude.1"), crs = 4326, remove = FALSE)

# Contextual map background
states_sf   <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
west_states <- c("california", "oregon", "washington", "idaho", "montana", "wyoming",
                 "nevada", "utah", "arizona", "colorado", "new mexico")
west_sf     <- states_sf[states_sf$ID %in% west_states, ]

# Pre-attack metrics (including names & fuel types)
bus_load_pre <- mpc_bus %>% select(bus_i, Pd, Vm, bus_name)
bus_gen_pre  <- mpc_gen %>%
  group_by(bus) %>%
  summarise(
    total_gen   = sum(Pg, na.rm = TRUE),
    max_capacity= sum(Pmax, na.rm = TRUE),
    fuel_type   = paste(unique(fuel_type), collapse = "/")
  ) %>%
  rename(bus_i = bus)

bus_info_pre <- bus_load_pre %>%
  left_join(bus_gen_pre, by = "bus_i") %>%
  mutate(
    total_gen    = ifelse(is.na(total_gen), 0, total_gen),
    gen_load_ratio = total_gen / (Pd + 1e-3),
    bus_type = case_when(
      total_gen > 0 & Pd > 0 ~ "Gen + Load",
      total_gen > 0          ~ "Generator",
      Pd       > 0           ~ "Load",
      TRUE                   ~ "Neither"
    )
  )

# Color palettes
bus_pal <- colorFactor(
  palette = c(
    "Generator"   = "#e41a1c",
    "Load"        = "#377eb8",
    "Gen + Load"  = "#4daf4a",
    "Neither"     = "#999999"
  ),
  domain = bus_info_pre$bus_type
)
fuel_pal <- colorFactor(
  palette = c(
    hydro   = "#1f78b4",
    wind    = "#a6cee3",
    solar   = "#feb24c",
    ng      = "#fc8d59",
    coal    = "#636363",
    nuclear = "#756bb1",
    UN      = "#b2df8a"
  ),
  domain = unique(mpc_gen$fuel_type)
)

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

set.seed(123)
steps <- 20
center_lon <- runif(1, -122, -105)
center_lat <- runif(1, 33, 45)
synthetic_perims <- generate_growing_fire(center_lon, center_lat, steps, max_radius_km = 300)
graph_sequence <- simulate_fire_cascade(graph_original, buses_sf, synthetic_perims, steps = steps)

resilience_metrics_df <- lapply(seq_along(graph_sequence), function(step) {
  graph <- graph_sequence[[step]]
  comps <- components(graph)
  in_service_buses <- as.numeric(V(graph)$name)
  
  bus_info_post <- bus_info_pre %>%
    mutate(
      is_active = bus_i %in% in_service_buses,
      load_lost = ifelse(!is_active, Pd, 0),
      gen_lost  = ifelse(!is_active, total_gen, 0)
    )
  total_load <- sum(bus_info_post$Pd, na.rm = TRUE)
  total_gen  <- sum(bus_info_post$total_gen, na.rm = TRUE)
  load_served<- sum(bus_info_post$Pd[bus_info_post$is_active], na.rm = TRUE)
  gen_served <- sum(bus_info_post$total_gen[bus_info_post$is_active], na.rm = TRUE)
  
  data.frame(
    step            = step,
    islands         = comps$no,
    largest_island  = max(comps$csize),
    load_served_pct = 100 * load_served / total_load,
    gen_served_pct  = 100 * gen_served / total_gen,
    buses_active    = length(in_service_buses),
    buses_lost      = nrow(bus_info_pre) - length(in_service_buses)
  )
}) %>% do.call(rbind, .)

ui <- fluidPage(
  titlePanel("Interactive Catastrophic Fire Grid Explorer"),
  leafletOutput("map", height = "750px"),
  absolutePanel(
    top = 10, right = 10, width = 300,
    sliderInput("step", "Step", min = 1, max = steps, value = 1, step = 1, animate = animationOptions(1200)),
    plotOutput("resilience_plot", height = "300px"),
    verbatimTextOutput("resilience_metrics")
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(-125, 31, -102, 49)
  })
  
  output$resilience_plot <- renderPlot({
    ggplot(resilience_metrics_df, aes(x = step)) +
      geom_line(aes(y = load_served_pct, color = "Load Served")) +
      geom_line(aes(y = gen_served_pct, color = "Generation Served")) +
      geom_line(aes(y = 100 * (1 - buses_lost / nrow(bus_info_pre)), color = "Buses Active")) +
      scale_color_manual(values = c("Load Served" = "blue", "Generation Served" = "green", "Buses Active" = "orange")) +
      labs(title = "Resilience Metrics Over Fire Progression",
           x = "Simulation Step", y = "% Value",
           color = "Metric") +
      theme_minimal()
  })
  
  output$resilience_metrics <- renderPrint({
    row <- resilience_metrics_df[input$step, ]
    cat(
      "Step:", row$step, "\n",
      "----------------------------\n",
      "Islands:", row$islands, "\n",
      "Largest Island Size:", row$largest_island, "\n\n",
      "Network Resilience:\n",
      "----------------------------\n",
      "Total Buses Lost:", row$buses_lost, "of", nrow(bus_info_pre), "\n",
      "Load Served:", round(row$load_served_pct, 1), "%\n",
      "Generation Served:", round(row$gen_served_pct, 1), "%\n"
    )
  })
  
  observeEvent(input$step, {
    step <- input$step
    current_graph <- graph_sequence[[step]]
    
    # Buses: join info and labels
    buses_step_sf <- buses_sf %>%
      filter(BusNum %in% as.numeric(V(current_graph)$name)) %>%
      left_join(bus_info_pre, by = c("BusNum" = "bus_i")) %>%
      mutate(
        bus_color = bus_pal(bus_type),
        bus_label = paste0(
          "<strong>Bus ", BusNum, " - ", bus_name, "</strong><br/>",
          "Type: ", bus_type, "<br/>",
          "Load (Pd): ", Pd, " MW<br/>",
          "Generation: ", total_gen, " MW"
        )
      )
    
    # Edges (branches)
    edges_step_sf <- prepare_edges_sf(current_graph, bus_data)
    
    # Generators: join bus names & assign popup
    gen_step_sf <- mpc_gen %>%
      filter(bus %in% as.numeric(V(current_graph)$name)) %>%
      left_join(mpc_bus %>% select(bus_i, bus_name), by = c("bus" = "bus_i")) %>%
      left_join(bus_data, by = c("bus" = "BusNum")) %>%
      st_as_sf(coords = c("Longitude.1", "Latitude.1"), crs = 4326, remove = FALSE) %>%
      mutate(
        gen_label = paste0(
          "<strong>Gen at Bus ", bus, " - ", bus_name, "</strong><br/>",
          "Type: ", fuel_type, "<br/>",
          "P<sub>g</sub>: ", Pg, " MW<br/>",
          "Max Cap: ", Pmax, " MW"
        )
      )
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolylines(
        data    = edges_step_sf,
        color   = "steelblue",
        weight  = 2,
        opacity = 0.7
      ) %>%
      addPolygons(
        data        = st_sf(geometry = synthetic_perims[step]),
        fillColor   = "orange",
        color       = "red",
        fillOpacity = 0.4,
        weight      = 1,
        popup       = paste0("Wildfire perimeter, step ", step)
      ) %>%
      addCircleMarkers(
        data = buses_step_sf,
        radius = 5,
        color = ~bus_color,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = lapply(buses_step_sf$bus_label, HTML),
        popup = ~bus_label
      ) %>%
      addCircleMarkers(
        data = gen_step_sf,
        radius = 6,
        fillColor = ~fuel_pal(fuel_type),
        color = "#000000",
        weight = 0.5,
        fillOpacity = 0.9,
        popup = ~gen_label
      ) %>%
      addLegend(
        position = "topright",
        pal      = fuel_pal,
        values   = mpc_gen$fuel_type,
        title    = "Generator Fuel Type",
        opacity  = 0.8
      ) %>%
      addLegend(
        "bottomright", 
        pal    = bus_pal, 
        values = buses_step_sf$bus_type,
        title  = "Bus Type",
        opacity = 0.8
      )
  })
}

shinyApp(ui, server)