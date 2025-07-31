library(multinet)
library(igraph)
library(TDAstats)

# Multi-Network Construction
create_multilayer_infrastructure_network <- function() {
  cat("Section 1: Creating Multi-layer Network\n")

  num_nodes <- 10
  layers <- c('energy', 'transportation', 'telecom')

  # Create an empty multilayer network
  ml_net <- ml_empty()

  # Add layers
  for (layer in layers) {
    g <- make_empty_graph(n = num_nodes, directed = FALSE)
    V(g)$name <- 1:num_nodes
    ml_net <- add_layer(ml_net, g, layer)
  }

  # Add intra-layer edges
  for (layer in layers) {
    for (i in 1:(num_nodes - 1)) {
      for (j in (i + 1):num_nodes) {
        if (runif(1) < 0.3) {
          ml_net <- add_edges_w(ml_net, data.frame(i, j, layer, 1))
        }
      }
    }
  }

  # Add inter-layer edges (dependencies)
  # Example: Telecom node depends on Energy node
  inter_layer_edges <- data.frame()
  for (i in 1:num_nodes) {
    if (runif(1) < 0.5) {
      # actor_from, layer_from, actor_to, layer_to, weight
      edge <- c(i, 'telecom', i, 'energy', 1)
      inter_layer_edges <- rbind(inter_layer_edges, edge)
    }
  }
  if (nrow(inter_layer_edges) > 0) {
    names(inter_layer_edges) <- c("actor_from", "layer_from", "actor_to", "layer_to", "weight")
    ml_net <- add_interlayer_edges(ml_net, inter_layer_edges)
  }

  cat(paste("Network created with", length(V(ml_net)), "nodes and", ecount(ml_net), "edges across 3 layers.\n\n"))
  return(ml_net)
}