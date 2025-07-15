library(igraph)
library(sf)
library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)
library(shiny)

# -- Load data and process (adjust paths as needed) --
graph_original <- read_graph("10k_bus_grid.gml", format = "gml")
bus_data <- read.csv("10k_buses.csv", stringsAsFactors = FALSE)
fires_sf <- st_read("PATH/TO/Perimeters.shp")
fires_sf <- st_make_valid(fires_sf)
fires_sf <- st_transform(fires_sf, crs = 4326)
states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
west_states <- c("california", "oregon", "washington", "idaho", "montana",
                 "wyoming", "nevada", "utah", "arizona", "colorado", "new mexico")
west_sf <- states_sf[states_sf$ID %in% west_states, ]
bbox_west <- st_bbox(c(xmin = -125, xmax = -102, ymin = 31, ymax = 49), crs = st_crs(fires_sf))
fires_west_sf <- st_crop(fires_sf, bbox_west)
buses_sf <- st_as_sf(bus_data, coords = c("Longitude.1", "Latitude.1"), crs = 4326, remove = FALSE)

# -- Simulation functions --
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
    fire_poly <- fire_polys[i]
    affected_buses <- buses_sf$BusNum[st_within(buses_sf, fire_poly, sparse = FALSE)]
    g_new <- simulate_attack(graphs[[i]], buses_to_remove = affected_buses)
    comp <- components(g_new)
    disconnected_buses <- V(g_new)$name[comp$membership != which.max(comp$csize)]
    g_new <- delete_vertices(g_new, disconnected_buses)
    graphs[[i + 1]] <- g_new
  }
  graphs
}
prepare_geometry <- function(g, bus_data) {
  vdf <- data.frame(
    BusNum = V(g)$name,
    Latitude = bus_data$Latitude.1[match(V(g)$name, bus_data$BusNum)],
    Longitude = bus_data$Longitude.1[match(V(g)$name, bus_data$BusNum)]
  )
  vdf <- na.omit(vdf)
  vertices_sf <- st_as_sf(vdf, coords = c("Longitude", "Latitude"), crs = 4326)
  edf <- igraph::as_data_frame(g, what = "edges")
  edf$lon1 <- bus_data$Longitude.1[match(edf$from, bus_data$BusNum)]
  edf$lat1 <- bus_data$Latitude.1[match(edf$from, bus_data$BusNum)]
  edf$lon2 <- bus_data$Longitude.1[match(edf$to, bus_data$BusNum)]
  edf$lat2 <- bus_data$Latitude.1[match(edf$to, bus_data$BusNum)]
  edf <- na.omit(edf)
  list(vertices = vertices_sf, edges = edf)
}

# -- Run simulation --
steps <- min(20, nrow(fires_west_sf))
fire_sequence <- fires_west_sf$geometry
graph_sequence <- simulate_fire_cascade(graph_original, buses_sf, fire_sequence, steps = steps)

# -- Shiny UI & Server --
ui <- fluidPage(
  titlePanel("Original Wildfire Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("step", "Wildfire Step:", 1, steps + 1, 1,
                  animate = animationOptions(interval = 1200, loop = FALSE)),
      helpText("Slide through real wildfire perimeter steps.")
    ),
    mainPanel(plotOutput("gridPlot", height = "700px"))
  )
)
server <- function(input, output) {
  output$gridPlot <- renderPlot({
    step <- input$step
    geo <- prepare_geometry(graph_sequence[[step]], bus_data)
    ggplot() +
      geom_sf(data = west_sf, fill = "gray95", color = "gray80") +
      geom_segment(data = geo$edges, aes(x = lon1, y = lat1, xend = lon2, yend = lat2),
                   color = "steelblue", linewidth = 0.3, alpha = 0.7) +
      geom_sf(data = geo$vertices, color = "firebrick", size = 1) +
      geom_sf(data = st_geometry(fires_west_sf)[step], fill = "orange", alpha = 0.5, color = "red") +
      coord_sf(xlim = c(-125, -102), ylim = c(31, 49), expand = FALSE) +
      labs(title = paste("Wildfire & Grid - Step", step), caption = "Orange: real fire perimeter")
  })
}
shinyApp(ui, server)