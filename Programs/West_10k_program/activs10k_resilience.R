rm(list = ls())

setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")

# activs10k_resilience_assessment.R
#
# This script focuses on analyzing the structural resilience of a power grid network
# represented by a graph. It begins by loading a pre-processed power grid graph from a
# GML file and its associated bus and branch data from CSV files.
#
# Author: Brandon Calvario

library(igraph)
library(sf)
library(ggplot2)
library(maps)
library(parallel)

start.time = Sys.time()


graph_original <- read_graph("10k_bus_grid.gml", format = "gml")
bus_data       <- read.csv("10k_buses.csv", stringsAsFactors = FALSE)
branch_data    <- read.csv("10k_branches.csv", stringsAsFactors = FALSE)

#' @title Simulate Attack
#' @description Removes specified vertices (buses) and edges (branches) from a graph.
#' @param g The input igraph object.
#' @param buses_to_remove A vector of vertex names (BusNum) to be removed.
#' @param branches_to_remove A vector of edge labels (e.g., "F_1") to be removed.
#' @return A new igraph object representing the grid after the attack.

simulate_attack <- function(g,
                            buses_to_remove   = NULL,
                            branches_to_remove= NULL) 
{
  if (!is.null(buses_to_remove)) {
    g <- delete_vertices(g, buses_to_remove)
  }
  if (!is.null(branches_to_remove)) {
    e_rm <- which(E(g)$label %in% branches_to_remove)
    if (length(e_rm)) g <- delete_edges(g, e_rm)
  }
  simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
}


#' @title Measure Resilience
#' @description Calculates the size of the largest connected component in the graph.
#' @param g The input igraph object.
#' @return An integer representing the number of vertices in the largest component.

measure_resilience <- function(g) {
  ug   <- as.undirected(g, mode = "collapse")
  comp <- components(ug)
  max(comp$csize)
}

# Random attack example
set.seed(123)
random_bus_attack    <- sample(V(graph_original)$name, 100)
random_branch_attack <- sample(E(graph_original)$label, 200)

attacked_graph <- simulate_attack(
  graph_original,
  buses_to_remove    = random_bus_attack,
  branches_to_remove = random_branch_attack
)
# Measure resilience
largest_component_size <- measure_resilience(attacked_graph)
cat("Largest Component Size after random attack:", 
    largest_component_size, "\n")

#' @title Prepare Geometry
#' @description Extracts and formats vertex and edge data for spatial plotting.
#' @param g An igraph object.
#' @return A list containing a spatial features data frame for vertices ('vertices')
#' and a data frame with coordinates for edges ('edges').

prepare_geometry <- function(g) {
  vdf <- data.frame(
    label     = vertex_attr(g, "BusName"),
    Latitude  = as.numeric(vertex_attr(g, "Latitude1")),
    Longitude = as.numeric(vertex_attr(g, "Longitude1"))
  )
  
  # Keep rows with complete Latitude and Longitude
  vdf <- vdf[complete.cases(vdf[, c("Latitude","Longitude")]), ]
  
  vertices_sf <- st_as_sf(vdf, coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Edges preparation (ensure the edge dataframe includes spatial coordinates)
  edf <- as_data_frame(g, what = "edges")
  edf$lon1 <- as.numeric(vertex_attr(g, "Longitude1")[match(edf$from, V(g)$name)])
  edf$lat1 <- as.numeric(vertex_attr(g, "Latitude1")[match(edf$from, V(g)$name)])
  edf$lon2 <- as.numeric(vertex_attr(g, "Longitude1")[match(edf$to, V(g)$name)])
  edf$lat2 <- as.numeric(vertex_attr(g, "Latitude1")[match(edf$to, V(g)$name)])
  edf <- edf[complete.cases(edf[, c("lat1","lon1","lat2","lon2")]), ]
  
  return(list(vertices = vertices_sf, edges = edf))
}
# Prepare map
states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
west_states <- c(
  "california", "oregon", "washington",
  "idaho", "montana", "wyoming",
  "nevada", "utah", "arizona",
  "colorado", "new mexico"
)
# Filter map
west_sf <- states_sf[ states_sf$ID %in% west_states, ]

#' @title Visualize Graph on Map
#' @description Creates a ggplot object showing the power grid overlaid on a map of the Western US.
#' @param g The igraph object to plot.
#' @return A ggplot object.

visualize_graph_west <- function(g) {
  geo <- prepare_geometry(g)
  
  ggplot() +
    geom_sf(data = west_sf, fill = "gray95", color = "gray70") +
    geom_segment(
      data    = geo$edges,
      aes(x = lon1, y = lat1, xend = lon2, yend = lat2),
      color   = "steelblue", size = 0.2, alpha = 0.4
    ) +
    geom_sf(
      data  = geo$vertices,
      color = "firebrick", size = 0.6
    ) +
    coord_sf(
      xlim = c(-125, -102),  # approximate longitudes for Western U.S.
      ylim = c(31,   49)     # approximate latitudes
    ) +
    labs(title = "10k-Bus Grid After Attack (Western U.S.)") +
    theme_minimal()
}

# Main and printing out results and example visualization
set.seed(123)
attacked <- simulate_attack(
  graph_original,
  buses_to_remove    = sample(V(graph_original)$name, 100),
  branches_to_remove = sample(E(graph_original)$label, 200)
)
print(measure_resilience(attacked))
print( visualize_graph_west(attacked) )

end.time = Sys.time()
time.taken = end.time - start.time