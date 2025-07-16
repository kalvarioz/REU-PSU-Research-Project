rm(list = ls())
setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")

library(parallelDist)
library(igraph)
library(TDA)
library(parallel)
library(ggplot2)

ensure_numeric <- function(x) {
  as.numeric(as.character(unlist(x)))
}

parse_activsg10k_aux <- function(aux_file_path) {
  lines_from_file <- readLines(aux_file_path)
  lines_from_file <- trimws(lines_from_file)
  
  get_section <- function(start_pattern, lines) {
    start_line   <- grep(start_pattern, lines, perl=TRUE)[1]
    if (is.na(start_line)) stop(paste("Section", start_pattern, "not found."))
    next_section <- grep("^DATA \\(", lines[(start_line + 1):length(lines)], perl=TRUE)
    end_line     <- if (length(next_section)==0) length(lines) else (start_line + next_section[1] - 1)
    block        <- lines[(start_line + 1):(end_line - 1)]
    block        <- block[grepl("^[0-9]+", block)]
    return(block)
  }
  # Bus
  parse_bus_line <- function(line) {
    pattern <- '^\\s*(\\d+)\\s+"([^"]+)"\\s+(\\S+)\\s+"([^"]+)"\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)'
    parts <- regmatches(line, regexec(pattern, line))[[1]]
    if (length(parts) == 17) {
      return(parts[2:17])
    } else {
      return(rep(NA, 16))
    }
  }
  bus_block  <- get_section("^DATA \\(Bus,", lines_from_file)
  bus_parsed <- t(sapply(bus_block, parse_bus_line))
  bus_df <- as.data.frame(bus_parsed, stringsAsFactors = FALSE)
  colnames(bus_df) <- c("BusNum", "BusName", "BusNomVolt", "BusSlack", 
                        "Bus:B1", "BusG:1", "BusPUVolt", "BusAngle",
                        "DCLossMultiplier", "AreaNum", "ZoneNum", "BANumber", 
                        "OwnerNum", "SubNum", "Latitude:1", "Longitude:1")
  # Branch
  parse_branch_line <- function(line) {
    pattern <- '("[^"]*"|\\S+)'
    parts <- unlist(regmatches(line, gregexpr(pattern, line))[[1]])
    parts <- gsub('^"|"$', '', trimws(parts))
    if (length(parts) >= 15) {
      return(parts[1:15])
    } else {
      return(rep(NA, 15))
    }
  }
  branch_block <- get_section("^DATA \\(Branch,", lines_from_file)
  branch_parsed <- t(sapply(branch_block, parse_branch_line))
  branch_df <- as.data.frame(branch_parsed, stringsAsFactors = FALSE)
  colnames(branch_df) <- c("BusNum", "BusNum:1", "LineCircuit", "BranchDeviceType", 
                           "ConsolidateBranch", "LineStatus", "NormLineStatus", 
                           "SeriesCapStatus", "LineMeter:1", "LineR", "LineX",
                           "LineC", "LineG", "LineLength", "LineMonEle")
  all_names_str <- c(
    "BusNum","GenID","GenStatus","GenVoltSet","GenRegNum","GenRMPCT","GenAGCAble","GenParFac",
    "GenMWSetPoint","GenMWMax","GenMWMin","GenEnforceMWLimits","GenAVRAble","GenMvrSetPoint",
    "GenMVRMax","GenMVRMin","GenUseCapCurve","GenWindControlMode","GenWindPowerFactor",
    "GenUseLDCRCC","GenRLDCRCC","GenXLDCRCC","GenMVABase","GenZR","GenZX","GenStepR","GenStepX",
    "GenStepTap","TSGovRespLimit","GenUnitType.1","AreaNum","ZoneNum","BANumber","OwnerNum",
    "OwnPercent","OwnerNum.1","OwnPercent.1","OwnerNum.2","OwnPercent.2","OwnerNum.3",
    "OwnPercent.3","OwnerNum.4","OwnPercent.4","OwnerNum.5","OwnPercent.5","OwnerNum.6",
    "OwnPercent.6","OwnerNum.7","OwnPercent.7","EMSType","EMSDeviceID","DataMaintainerAssign",
    "AllLabels","GenUnitType","GenTotalFixedCosts","GenCostModel","GenFuelType","GenFuelCost",
    "GenFixedCost","GenIOD","GenIOC","GenIOB"
    
 
  )
  
  parse_gen_line <- function(line) {
    tokens <- unlist(regmatches(line, gregexpr('("[^"]*"|\\S+)', line, perl=TRUE)))
    tokens <- gsub('^"|"$', '', tokens)
    if (length(tokens) != length(gen_colnames)) {
      warning("Gen line token count ", length(tokens),
              " != ", length(gen_colnames), ": ", line)
      tokens <- c(tokens, rep(NA, length(gen_colnames)))[1:length(gen_colnames)]
    }
    return(tokens)
  }
  gen_colnames <- strsplit(all_names_str, ",")[[1]]
  gen_block  <- get_section("^DATA \\(Gen,", lines_from_file)
  gen_parsed <- t(sapply(gen_block, parse_gen_line, USE.NAMES=FALSE))
  generator_df <- as.data.frame(gen_parsed, stringsAsFactors=FALSE)
  colnames(generator_df) <- gen_colnames
  
  stopifnot(
    nrow(generator_df)==length(gen_block),
    ncol(generator_df)==length(gen_colnames)
  )
  
  generator_df$GenMWSetPoint <- ensure_numeric(generator_df$GenMW)
  generator_df$GenMVR <- ensure_numeric(generator_df$GenMVR)
  
  all_load_names <- c(
    "BusNum","LoadID","LoadStatus","GenAGCAble","LoadSMW","LoadSMVR","LoadIMW","LoadIMVR",
    "LoadZMW","LoadZMVR","DistStatus","DistMWInput","DistMvarInput","Interruptible",
    "GenMWMax","GenMWMin","LoadModelGroup","AreaNum","ZoneNum","BANumber","OwnerNum",
    "EMSType","EMSDeviceID","DataMaintainerAssign","AllLabels"
  )

  parse_load_line <- function(line) {
    tokens <- unlist(regmatches(line, gregexpr('("[^"]*"|\\S+)', line, perl=TRUE)))
    tokens <- gsub('^"|"$', '', tokens)
    if (length(tokens) != length(load_colnames)) {
      warning("Load line token count ", length(tokens),
              " != ", length(load_colnames), ": ", line)
      tokens <- c(tokens, rep(NA, length(load_colnames)))[1:length(load_colnames)]
    }
    return(tokens)
  }
  
  load_block  <- get_section("^DATA \\(Load,", lines_from_file)
  load_parsed <- t(sapply(load_block, parse_load_line, USE.NAMES=FALSE))
  load_df     <- as.data.frame(load_parsed, stringsAsFactors=FALSE)
  load_colnames <- trimws(strsplit(all_load_names, ",")[[1]])
  colnames(load_df) <- load_colnames
  
  stopifnot(
    nrow(load_df)==length(load_block),
    ncol(load_df)==length(load_colnames)
  )
  
  load_df$LoadMW  <- ensure_numeric(load_df$LoadMW)
  load_df$LoadMVR <- ensure_numeric(load_df$LoadMVR)
  return(list(
    bus_df       = bus_df,
    branch_df    = branch_df,
    generator_df = generator_df,
    load_df      = load_df
  ))
}

add_environmental_attributes <- function(graph) {
  set.seed(123)
  V(graph)$HeatScore <- runif(vcount(graph), 0, 1)
  V(graph)$RainScore <- runif(vcount(graph), 0, 1)
  V(graph)$Region <- sample(c("North", "South", "Coast", "Urban", "Rural"), vcount(graph), replace = TRUE)
  V(graph)$Elevation <- runif(vcount(graph), 0, 1000)
  E(graph)$WindFragility <- runif(ecount(graph), 0, 1)
  E(graph)$LineAge <- sample(1:50, ecount(graph), replace = TRUE)
  E(graph)$Length_km <- runif(ecount(graph), 1, 10)
  return(graph)
}

build_igraph_from_aux <- function(bus_df, branch_df, generator_df, load_df) {
  # Ensure character types for keys
  bus_df$BusNum <- as.character(bus_df$BusNum)
  branch_df$BusNum <- as.character(branch_df$BusNum)
  branch_df[["BusNum:1"]] <- as.character(branch_df[["BusNum:1"]])
  generator_df$BusNum <- as.character(generator_df$BusNum)
  load_df$BusNum <- as.character(load_df$BusNum)
  
  # Ensure numeric for aggregations
  generator_df$GenMW <- ensure_numeric(generator_df$GenMW)
  load_df$LoadMW <- ensure_numeric(load_df$LoadMW)
  
  # Create edge list for igraph
  edge_list <- data.frame(
    from = branch_df$BusNum,
    to = branch_df[["BusNum:1"]],
    label = paste0("F_", seq_len(nrow(branch_df))),
    stringsAsFactors = FALSE
  )
  g <- graph_from_data_frame(edge_list, directed = TRUE, vertices = bus_df)
  V(g)$name <- bus_df$BusNum
  
  # Aggregate and assign generator/load data to buses
  V(g)$GenMW <- 0
  gen_agg <- aggregate(generator_df$GenMW, by = list(generator_df$BusNum), sum, na.rm=TRUE)
  names(gen_agg) <- c("BusNum", "TotalGenMW")
  idx_gen <- match(gen_agg$BusNum, V(g)$name)
  V(g)$GenMW[idx_gen] <- gen_agg$TotalGenMW
  
  V(g)$LoadMW <- 0
  load_agg <- aggregate(load_df$LoadMW, by = list(load_df$BusNum), sum, na.rm=TRUE)
  names(load_agg) <- c("BusNum", "TotalLoadMW")
  idx_load <- match(load_agg$BusNum, V(g)$name)
  V(g)$LoadMW[idx_load] <- load_agg$TotalLoadMW
  
  return(g)
}
create_adjacency_matrix <- function(bus_df, branch_df) {
  bus_nums <- unique(bus_df$BusNum)
  n <- length(bus_nums)
  adj_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(bus_nums, bus_nums))
  for (i in 1:nrow(branch_df)) {
    from_bus <- branch_df$BusNum[i]
    to_bus <- branch_df[["BusNum:1"]][i]
    if (from_bus %in% bus_nums && to_bus %in% bus_nums) {
      adj_matrix[from_bus, to_bus] <- 1
      adj_matrix[to_bus, from_bus] <- 1
    }
  }
  return(adj_matrix)
}

write_perseus_distance_matrix <- function(dist_matrix, file_name = "perseus_distmat.txt") {
  n <- nrow(dist_matrix)
  file_conn <- file(file_name, open = "wt")
  writeLines(as.character(n), file_conn)
  for (i in 1:(n-1)) {
    line <- paste(dist_matrix[i, (i+1):n], collapse = " ")
    writeLines(line, file_conn)
  }
  close(file_conn)
}

load_perseus_barcodes <- function(file_path) {
  barcode_data <- read.table(file_path, header = FALSE, col.names = c("Birth", "Death"))
  return(barcode_data)
}

aux_file_path <- "ACTIVSg10k.aux"
aux_data <- parse_activsg10k_aux(aux_file_path)

# Build graph correctly
g2000 <- build_igraph_from_aux(
  aux_data$bus_df,
  aux_data$branch_df,
  aux_data$generator_df,
  aux_data$load_df
)

# Add environmental attributes
g2000 <- add_environmental_attributes(g2000)

# Compute net power attribute
V(g2000)$NetPowerMW <- V(g2000)$GenMW - V(g2000)$LoadMW

# CSV Exports
write.csv(aux_data$bus_df, "10k_buses.csv", row.names = FALSE)
write.csv(aux_data$branch_df, "10k_branches.csv", row.names = FALSE)
write.csv(aux_data$generator_df, "10k_generators.csv", row.names = FALSE)
write.csv(aux_data$load_df, "10k_loads.csv", row.names = FALSE)
write_graph(g2000, "10k_bus_grid.gml", format = "gml")

# Export CSV explicitly for PowerWorld
bus_df_export <- aux_data$bus_df[, c("BusNum", "BusPUVolt", "BusAngle", "Latitude:1", "Longitude:1")]
bus_df_export$BusPUVolt <- ensure_numeric(bus_df_export$BusPUVolt)
bus_df_export$BusAngle <- ensure_numeric(bus_df_export$BusAngle)
write.csv(bus_df_export, "bus_export_for_PowerWorld.csv", row.names = FALSE)

# TDA computations
adjacency_matrix <- create_adjacency_matrix(aux_data$bus_df, aux_data$branch_df)
write.csv(adjacency_matrix, "bus_adjacency_matrix.csv")

# Correct distance calculation
dist_matrix <- distances(g2000, mode = "all")
dist_matrix[is.infinite(dist_matrix)] <- max(dist_matrix[is.finite(dist_matrix)]) + 1

# Write Perseus file
write_perseus_distance_matrix(dist_matrix, "perseus_distmat.txt")

# Ensure Perseus is correctly referenced
system2("./perseus", args = c("distmat", "perseus_distmat.txt", "perseus_output"))

# Run Persistent Homology directly (without redundant parallelDist)
tda_result <- ripsDiag(
  X            = dist_matrix,
  maxdimension = 2,
  maxscale     = 5,
  dist         = "arbitrary",
  library      = "GUDHI",
  printProgress= TRUE
)

# Load barcodes and visualize
h1_barcodes <- load_perseus_barcodes("perseus_output_1.txt")

ggplot(h1_barcodes, aes(x = Birth, xend = Death, y = seq_along(Birth), yend = seq_along(Death))) +
  geom_segment(linewidth = 1.2, color = "blue") +
  labs(title = "H1 Persistent Homology Barcodes", x = "Filtration Parameter", y = "Features") +
  theme_minimal()

plot(tda_result[["diagram"]], main = "Persistent Homology of Grid")
save(tda_result, file = "tda_original_state.RData")
