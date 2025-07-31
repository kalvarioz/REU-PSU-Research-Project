rm(list = ls())
start.time = Sys.time()

library(igraph)
library(sf)
library(ggplot2)
library(maps)
library(parallel)

# Load graph globally
input_graph <- read_graph("2000_bus_grid.gml", format = "gml")

# Load result data globally ONCE
result_data <- read.csv("result-2000.csv")
bus_cols    <- sort(grep("^b_", names(result_data), value = TRUE),
                    index.return = FALSE, method = "shell")
branch_cols <- sort(grep("^F_", names(result_data), value = TRUE))

bus_name_map <- V(input_graph)$name         # 2000 entries, order == id+1

## 2) ATTACK ONE TIME STEP ---------------------------------------------
attack_one_step <- function(g, step_idx) {
  ## buses --------------------------------------------------------------
  bus_stat <- as.numeric(result_data[step_idx, bus_cols])
  buses_removed <- bus_name_map[ bus_stat == 0 ]
  g <- delete_vertices(g, buses_removed)
  
  ## branches -----------------------------------------------------------
  br_stat <- as.numeric(result_data[step_idx, branch_cols])
  edges_removed <- branch_cols[ br_stat == 0 ]     # labels "F_###"
  rm_edges <- which(E(g)$label %in% edges_removed)
  if (length(rm_edges)) g <- delete_edges(g, rm_edges)
  
  simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
}

## 3) PARALLEL LOOP OVER TIME STEPS ------------------------------------
library(parallel)
nSteps <- nrow(result_data)
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, list("input_graph","result_data",
                       "bus_cols","branch_cols","bus_name_map",
                       "attack_one_step"))
clusterEvalQ(cl, library(igraph))

GC_parallel <- parLapply(cl, 1:nSteps, function(t) {
  g_att <- attack_one_step(input_graph, t)
  ug    <- as.undirected(g_att, mode = "collapse")
  max(components(ug)$csize)
})
stopCluster(cl)

GC <- unlist(GC_parallel)
write.csv(data.frame(step = 1:nSteps, GC = GC),
          "GC_2000_bus_parallel_by_row.csv")

visualize_attack <- function(graph_attacked) {
    # Explicitly extract vertex attributes safely
    vertices_df <- data.frame(
      Longitude = as.numeric(vertex_attr(graph_attacked, "Longitude")),
      Latitude  = as.numeric(vertex_attr(graph_attacked, "Latitude")),
      label     = vertex_attr(graph_attacked, "BusName")
    )
    
    eds <- as_data_frame(graph_attacked, what = "edges")
    # Ensure coordinates are numeric and remove missing values
    vertices_df <- vertices_df[complete.cases(vertices_df[, c("Longitude", "Latitude")]), ]
    # Convert to sf object
    vertices_sf <- st_as_sf(vertices_df, coords = c("Longitude", "Latitude"), crs = 4326)
    # Texas map
    texas_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
    texas_sf <- texas_map[texas_map$ID == "texas", ]
    # Plot
  # Plot
  ggplot() +
    geom_sf(data = texas_sf, fill = "gray95", color = "gray70") +
    geom_segment(
      data    = edges_geo,
      aes(x = lon1, y = lat1, xend = lon2, yend = lat2),
      color   = "steelblue",
      size    = 0.3,
      alpha   = 0.5
    ) +
    geom_point(
      data    = verts,
      aes(x = lon, y = lat),
      color   = "firebrick",
      size    = 1
    ) +
    coord_sf(xlim = c(-107, -93), ylim = c(25, 37)) +
    labs(title = "Texas 2000-Bus Grid: Buses (red) & Lines (blue)") +
    theme_minimal()
}

graph_after_attack <- attack_one_step(input_graph, 113)
visualize_attack(graph_after_attack)