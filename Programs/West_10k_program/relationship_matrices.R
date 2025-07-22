library(igraph)
library(TDA)
library(ggplot2)

setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")

ensure_numeric <- function(x) as.numeric(as.character(unlist(x)))

bus_df <- read.csv("10k_buses.csv", stringsAsFactors=FALSE)
branch_df <- fread("10k_branches.csv",   stringsAsFactors=FALSE)
norm_br <- fread("mpc_branch.csv",      stringsAsFactors=FALSE)
graph <- read_graph("10k_bus_grid.gml", format="gml")
gen_data <- read.csv("gen_data.csv")
load_data <- read.csv("load_data.csv")

# Aggregate generation/load clearly
gen_agg <- aggregate(as.numeric(gen_data$GenMWSetPoint), 
                     by=list(BusNum=gen_data$BusNum), sum, na.rm=TRUE)
colnames(gen_agg)[2] <- "GenMW"

load_agg <- aggregate(as.numeric(load_data$LoadSMW), 
                      by=list(BusNum=load_data$BusNum), sum, na.rm=TRUE)
colnames(load_agg)[2] <- "LoadMW"

bus_df$BusNum <- as.character(bus_df$BusNum)
bus_df <- merge(bus_df, gen_agg, by="BusNum", all.x=TRUE)
bus_df <- merge(bus_df, load_agg, by="BusNum", all.x=TRUE)
bus_df$GenMW[is.na(bus_df$GenMW)] <- 0
bus_df$LoadMW[is.na(bus_df$LoadMW)] <- 0
bus_df$NetPowerMW <- bus_df$GenMW - bus_df$LoadMW

# Vertex attributes corrected
V(graph)$GenMW <- bus_df$GenMW[match(V(graph)$name, bus_df$BusNum)]
V(graph)$LoadMW <- bus_df$LoadMW[match(V(graph)$name, bus_df$BusNum)]
V(graph)$NetPowerMW <- bus_df$NetPowerMW[match(V(graph)$name, bus_df$BusNum)]

# Check/add environmental attributes explicitly if missing
if(!"HeatScore" %in% vertex_attr_names(graph)) {
  graph <- add_environmental_attributes(graph)
}

# Weighted adjacency
branch_df$LineX <- ensure_numeric(branch_df$LineX)
bus_nums <- bus_df$BusNum
n <- length(bus_nums)
weighted_adj <- matrix(0, n, n, dimnames=list(bus_nums, bus_nums))
for(i in seq_len(nrow(branch_df))) {
  u <- as.character(branch_df$BusNum[i])
  v <- as.character(branch_df$BusNum.1[i])
  X <- branch_df$LineX[i]
  if (!is.na(X) && X != 0) {
    weighted_adj[u, v] <- 1/X
    weighted_adj[v, u] <- 1/X
  }
}
write.csv(weighted_adj, "weighted_adjacency_matrix.csv")

# Voltage difference
voltages <- ensure_numeric(bus_df$BusPUVolt)
names(voltages) <- bus_nums
voltage_diff <- abs(outer(voltages, voltages, "-"))
write.csv(voltage_diff, "voltage_difference_matrix.csv")

# Net power difference
net_power <- V(graph)$NetPowerMW
names(net_power) <- V(graph)$name
net_power_diff <- abs(outer(net_power, net_power, "-"))
write.csv(net_power_diff, "net_power_difference_matrix.csv")

# Environmental similarity
env_data <- data.frame(
  Heat = V(graph)$HeatScore,
  Rain = V(graph)$RainScore,
  Elevation = V(graph)$Elevation
)
env_distance <- as.matrix(dist(env_data))
write.csv(env_distance, "environmental_distance_matrix.csv")

# Wind fragility
wind_fragility <- matrix(0, n, n, dimnames=list(bus_nums, bus_nums))
for(e in E(graph)) {
  u <- head_of(graph, e)$name
  v <- tail_of(graph, e)$name
  fragility <- E(graph)$WindFragility[e]
  wind_fragility[u,v] <- fragility
  wind_fragility[v,u] <- fragility
}
write.csv(wind_fragility, "wind_fragility_matrix.csv")

# Distance for TDA
graph_distances <- distances(graph, mode="all")
graph_distances[is.infinite(graph_distances)] <- max(graph_distances[is.finite(graph_distances)]) + 1
write.csv(graph_distances, "graph_distance_matrix.csv")