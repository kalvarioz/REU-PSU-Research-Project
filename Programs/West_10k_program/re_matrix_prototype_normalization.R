# west_10k_reimpl.R
# Re-implementation with merged normalized rateA and bus-attribute normalization

library(data.table)
library(igraph)
setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")

ensure_numeric <- function(x) as.numeric(as.character(unlist(x)))

## ─── 1) Read & merge branch data with normalized rateA ───────────────────────

# Original branch data (with LineX, etc.)
orig_br <- fread("parsed_normalized_csv/branch_data.csv", stringsAsFactors=FALSE)
print(names(orig_br))

# show the first 6 rows
print(head(orig_br))

# MATPOWER branch export (already overwritten) with normalized rateA, fbus/tbus
norm_br <- fread("mpc_branch.csv", stringsAsFactors=FALSE)


# Align column names for join
setnames(norm_br,
         old = c("fbus","tbus"),
         new = c("BusNum","BusNum.1"))

# Keep only rateA plus identifiers
join_cols <- c("BusNum","BusNum.1")
orig_br[,  (join_cols) := lapply(.SD, as.character), .SDcols = join_cols]
norm_br[,  (join_cols) := lapply(.SD, as.character), .SDcols = join_cols]

# Left-join normalized rateA onto the original branch table
branch_df <- merge(orig_br,
                   norm_br[, .(BusNum, BusNum.1, rateA = as.numeric(rateA))],
                   by = join_cols,
                   all.x = TRUE)
branch_df[is.na(rateA), rateA := 0]


sapply(orig_br[, .(BusNum, `BusNum.1`)], class)
sapply(norm_br[, .(BusNum, `BusNum.1`)], class)

# glimpse the first few rows
head(orig_br)
head(norm_br)

print(names(norm_br))
print(head(norm_br))
# peek at the raw header line
## ─── 2) Read buses, gens, loads, graph; aggregate gen/load ────────────────────

bus_df    <- fread("parsed_normalized_csv/bus_data.csv",    stringsAsFactors=FALSE)
gen_data  <- fread("gen_data.csv",      stringsAsFactors=FALSE)
load_data <- fread("load_data.csv",     stringsAsFactors=FALSE)

# Aggregate per-bus generation/load
gen_agg  <- gen_data[, .(GenMW = sum(ensure_numeric(GenMWSetPoint), na.rm=TRUE)),
                     by=BusNum]
load_agg <- load_data[, .(LoadMW = sum(ensure_numeric(LoadSMW),     na.rm=TRUE)),
                      by=BusNum]

# Merge into bus_df
bus_df <- merge(bus_df, gen_agg,  by="BusNum", all.x=TRUE)
bus_df <- merge(bus_df, load_agg, by="BusNum", all.x=TRUE)
bus_df[is.na(GenMW),  GenMW  := 0]
bus_df[is.na(LoadMW), LoadMW := 0]
bus_df[, NetPowerMW := GenMW - LoadMW]

# Load graph and attach vertex attrs
graph <- read_graph("10k_bus_grid.gml", format="gml")
V(graph)$GenMW      <- bus_df$GenMW[match(V(graph)$name, bus_df$BusNum)]
V(graph)$LoadMW     <- bus_df$LoadMW[match(V(graph)$name, bus_df$BusNum)]
V(graph)$NetPowerMW <- bus_df$NetPowerMW[match(V(graph)$name, bus_df$BusNum)]

# Ensure environmental scores exist
if(!"HeatScore" %in% vertex_attr_names(graph)) {
  graph <- add_environmental_attributes(graph)
}

## ─── 3) Normalize bus-level attributes into [0,1] ─────────────────────────────

# helper for min–max
minmax <- function(x) (x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))

# Voltage (BusPUVolt)
bus_df[, BusPUVolt := ensure_numeric(BusPUVolt)]
bus_df[, BusPUVolt := minmax(BusPUVolt)]

# NetPowerMW
bus_df[, NetPowerMW := minmax(NetPowerMW)]

# Environmental scores
env_scores <- data.table(
  name      = V(graph)$name,
  HeatScore = ensure_numeric(V(graph)$HeatScore),
  RainScore = ensure_numeric(V(graph)$RainScore),
  Elevation = ensure_numeric(V(graph)$Elevation)
)
env_scores[, `:=`(
  HeatScore = minmax(HeatScore),
  RainScore = minmax(RainScore),
  Elevation = minmax(Elevation)
)]
bus_df[, BusNum := as.character(BusNum)]

# push back into bus_df for matrix builds
bus_df <- merge(
  bus_df,
  env_scores,
  by.x = "BusNum",
  by.y = "name",
  all.x = TRUE
)

## ─── 4) Build & write your matrices ─────────────────────────────────────────

bus_nums <- as.character(bus_df$BusNum)
n        <- length(bus_nums)

# 4a) Weighted adjacency from normalized rateA
W <- matrix(0, n, n, dimnames=list(bus_nums, bus_nums))
for(i in seq_len(nrow(branch_df))) {
  u <- as.character(branch_df$BusNum[i])
  v <- as.character(branch_df$BusNum.1[i])
  w <- branch_df$rateA[i]
  if(w > 0) {
    W[u,v] <- w
    W[v,u] <- w
  }
}
fwrite(as.data.table(W, keep.rownames="BusNum"), "weighted_adjacency_rateATEST.csv")

# 4b) Distance matrix for TDA (invert similarity)
D_rateA <- 1 - W
diag(D_rateA) <- 0
fwrite(as.data.table(D_rateA, keep.rownames="BusNum"), "rateA_distance_matrixTEST.csv")

# 4c) Voltage‐difference matrix
volt <- bus_df$BusPUVolt; names(volt) <- bus_nums
Vdiff <- abs(outer(volt, volt, "-"))
fwrite(as.data.table(Vdiff, keep.rownames="BusNum"), "voltage_difference_normalizedTEST.csv")

# 4d) Net‐power‐difference matrix
np <- bus_df$NetPowerMW; names(np) <- bus_nums
Pdiff <- abs(outer(np, np, "-"))
fwrite(as.data.table(Pdiff, keep.rownames="BusNum"), "net_power_difference_normalizedTEST.csv")

# 4e) Environmental‐distance matrix
env_mat <- as.matrix(env_scores[, .(HeatScore, RainScore, Elevation)])
EnvDist <- as.matrix(dist(env_mat))
fwrite(as.data.table(EnvDist, keep.rownames="BusNum"), "environmental_distance_normalizedTEST.csv")

# 4f) Wind fragility adjacency
wind <- matrix(0, n, n, dimnames=list(bus_nums, bus_nums))
for(e in E(graph)) {
  u <- head_of(graph, e)$name
  v <- tail_of(graph, e)$name
  f <- ensure_numeric(E(graph)$WindFragility[e])
  wind[u, v] <- f
  wind[v, u] <- f
}
fwrite(as.data.table(wind, keep.rownames="BusNum"), "wind_fragility_matrixTEST.csv")

# 4g) Graph‐based shortest‐path distances
Gdist <- distances(graph, mode="all")
# replace Inf with max finite +1 (so TDA sees a bound)
M <- max(Gdist[is.finite(Gdist)])
Gdist[!is.finite(Gdist)] <- M + 1
fwrite(as.data.table(Gdist, keep.rownames="BusNum"), "graph_distance_matrixTEST.csv")