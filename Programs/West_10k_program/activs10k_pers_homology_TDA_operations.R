rm(list = ls())

setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")

# activs10k_aux_to_csv.R
#
# This script is designed to parse a proprietary power grid data file (`.aux` format),
# specifically the 'ACTIVSg10k.aux' file. Its primary function is to extract structured
# data for two key components of a power grid: buses (nodes) and branches (edges).
#
# Author: Brandon Calvario

library(igraph)
library(TDA)

start.time = Sys.time()

aux_file_path <- "ACTIVSg10k.aux"

# explicitly read file lines
lines_from_file <- readLines(aux_file_path)

#' @title Parse ACTIVSg AUX File
#' @description Reads an ACTIVSg .aux file and extracts bus and branch data into data frames.
#' @param aux_file_path The file path to the .aux file.
#' @return A list containing two data frames: 'bus_df' for bus data and 'branch_df' for branch data.
parse_activsg2000_aux <- function(aux_file_path) {
  lines_from_file <- readLines(aux_file_path)
  lines_from_file <- trimws(lines_from_file)
  
  #' @title Get Section
  #' @description A helper function to extract a block of data from the file lines.
  #' @param start_pattern A regular expression that marks the beginning of the section.
  #' @param lines The vector of lines from the file.
  #' @return A character vector containing the lines of the desired section.
  get_section <- function(start_pattern, lines) {
    start_line <- grep(start_pattern, lines, perl=TRUE)[1]
    if (is.na(start_line)) stop(paste("Section", start_pattern, "not found."))
    next_section <- grep("^DATA \\(", lines[(start_line + 1):length(lines)], perl=TRUE)
    end_line <- if (length(next_section) == 0) length(lines) else (start_line + next_section[1] - 1)
    section_block <- lines[(start_line + 1):(end_line - 1)]
    section_block <- section_block[grepl("^[0-9]+", section_block)]
    return(section_block)
  }
  #' @title Parse Bus Line
  #' @description Parses a single line of bus data using a regular expression.
  #' @param line A string representing one line of bus data.
  #' @return A character vector of the parsed fields.
  parse_bus_line <- function(line) {
    # Using regular expressions clearly capturing all fields
    # TODO: Make this better, dont like the length, maybe something more efficient like what i did later on, but works for now.
    pattern <- '^\\s*(\\d+)\\s+"([^"]+)"\\s+(\\S+)\\s+"([^"]+)"\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)'
    parts <- regmatches(line, regexec(pattern, line))[[1]]
    if (length(parts) == 17) {
      return(parts[2:17])
    } else {
      return(rep(NA, 16))
    }
  }
  # Column names
  bus_block  <- get_section("^DATA \\(Bus,", lines_from_file)
  bus_parsed <- t(sapply(bus_block, parse_bus_line))
  bus_df <- as.data.frame(bus_parsed, stringsAsFactors = FALSE)
  colnames(bus_df) <- c("BusNum", "BusName", "BusNomVolt", "BusSlack", 
                        "Bus:B1", "BusG:1", "BusPUVolt", "BusAngle",
                        "DCLossMultiplier", "AreaNum", "ZoneNum", "BANumber", 
                        "OwnerNum", "SubNum", "Latitude:1", "Longitude:1")
  #' @title Parse Branch Line
  #' @description Parses a single line of branch data.
  #' @param line A string representing one line of branch data.
  #' @return A character vector of the parsed fields.
  parse_branch_line <- function(line) {
    # Regular expression to correctly capture quoted or unquoted fields
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
  colnames(branch_df) <- c("BusNum", "BusNum:1", "LineCircuit", "BranchDeviceType", "ConsolidateBranch", "LineStatus","NormLineStatus","SeriesCapStatus","LineMeter:1","LineR","LineX",
                           "LineC","LineG","LineLength","LineMonEle")
  return(list(bus_df = bus_df, branch_df = branch_df))
}

#' @title Add Environmental Attributes
#' @description Adds synthetic environmental data to the graph for simulation purposes.
#' @param graph An igraph object.
#' @return The igraph object with new vertex and edge attributes.

add_environmental_attributes <- function(graph) {
  set.seed(123)
  # Attributes for buses
  V(graph)$HeatScore <- runif(vcount(graph), 0, 1)
  V(graph)$RainScore <- runif(vcount(graph), 0, 1)
  V(graph)$Region <- sample(c("North", "South", "Coast", "Urban", "Rural"), vcount(graph), replace = TRUE)
  V(graph)$Elevation <- runif(vcount(graph), 0, 1000)
  # Attributes for branches
  E(graph)$WindFragility <- runif(ecount(graph), 0, 1)
  E(graph)$LineAge <- sample(1:50, ecount(graph), replace = TRUE)
  E(graph)$Length_km <- runif(ecount(graph), 1, 10)
  return(graph)
}
#' @title Build igraph from AUX data
#' @description Creates an igraph graph object from the parsed bus and branch data frames.
#' @param bus_df The data frame of buses (will become vertices).
#' @param branch_df The data frame of branches (will become edges).
#' @return An igraph object representing the power grid.
build_igraph_from_aux <- function(bus_df, branch_df) {
  
  # Verify that the branch data frame has the required 'from' and 'to' bus columns.
  required_cols <- c("BusNum", "BusNum:1")
  if (!all(required_cols %in% names(branch_df))) {
    stop(paste("Branch dataframe must contain columns:", paste(required_cols, collapse="")))
  }
  
  # Ensure character types for consistency
  branch_df$BusNum <- as.character(branch_df$BusNum)
  branch_df[["BusNum:1"]] <- as.character(branch_df[["BusNum:1"]])
  bus_df$BusNum <- as.character(bus_df$BusNum)
  
  # Create edge list (from, to) vertices
  edge_list <- data.frame(
    from = branch_df$BusNum,
    to = branch_df[["BusNum:1"]],
    label = paste0("F_", seq_len(nrow(branch_df))),
    stringsAsFactors = FALSE
  )
  
  # Construct igraph
  g <- graph_from_data_frame(edge_list, directed = TRUE, vertices = bus_df)
  V(g)$name <- bus_df$BusNum
  
  return(g)
}

# Main execution 
# Preforms parsing on the AUX file, to get bus and branch data.

aux_data <- parse_activsg2000_aux("ACTIVSg10k.aux")
#Verify
print(head(aux_data$bus_df))
print(head(aux_data$branch_df))
# Add synthetic attributes
g2000 <- build_igraph_from_aux(aux_data$bus_df, aux_data$branch_df)
g2000 <- add_environmental_attributes(g2000)
print(summary(g2000))

head(vertex_attr(g2000))
head(edge_attr(g2000))
# Write files 
write.csv(aux_data$bus_df, "10k_buses.csv", row.names = FALSE)
write.csv(aux_data$branch_df, "10k_branches.csv", row.names = FALSE)
write_graph(g2000, "10k_bus_grid.gml", format = "gml")

create_adjacency_matrix <- function(bus_df, branch_df) {
  bus_nums <- unique(bus_df$BusNum)
  n <- length(bus_nums)
  adj_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(bus_nums, bus_nums))
  
  for (i in 1:nrow(branch_df)) {
    from_bus <- branch_df$BusNum[i]
    to_bus <- branch_df[["BusNum:1"]][i]
    
    if (from_bus %in% bus_nums && to_bus %in% bus_nums) {
      adj_matrix[from_bus, to_bus] <- 1
      # If undirected graph, also include:
      adj_matrix[to_bus, from_bus] <- 1
    }
  }
  
  return(adj_matrix)
}

write_perseus_distance_matrix <- function(dist_matrix, file_name = "perseus_distmat.txt") {
  n <- nrow(dist_matrix)
  file_conn <- file(file_name, open = "wt")
  
  # First line must be number of vertices
  writeLines(as.character(n), file_conn)
  
  # Write upper triangular part (excluding diagonal) row-wise
  for (i in 1:(n-1)) {
    line <- paste(dist_matrix[i, (i+1):n], collapse = " ")
    writeLines(line, file_conn)
  }
  
  close(file_conn)
}

# Generate adjacency matrix from your AUX data
adjacency_matrix <- create_adjacency_matrix(aux_data$bus_df, aux_data$branch_df)
print(adjacency_matrix[1:10, 1:10]) # Quick check, adjust indexing as necessary
dist_matrix <- 1 - adjacency_matrix  # Simplified: assumes binary adjacency for TDA


# Save adjacency matrix as CSV for external reference
write.csv(adjacency_matrix, "bus_adjacency_matrix.csv")

# Now perform TDA operations, for instance Vietoris-Rips filtration and persistent homology:
write_perseus_distance_matrix(dist_matrix, "perseus_distmat.txt")
system("./perseus distmat perseus_distmat.txt perseus_output")


# Example using Rips filtration with 'ripsDiag' from the TDA package:
max_dimension <- 2      # Max dimension of homological features to compute
max_scale <- 5          # Maximum scale parameter for filtration (adjust as needed)

tda_result <- ripsDiag(
  X            = dist_matrix,
  maxdimension = max_dimension,
  maxscale     = max_scale,
  dist         = "arbitrary",
  library      = "Dionysus",
  printProgress= TRUE
)

load_perseus_barcodes <- function(file_path) {
  barcode_data <- read.table(file_path, header = FALSE, col.names = c("Birth", "Death"))
  return(barcode_data)
}

# Example loading H1 features
h1_barcodes <- load_perseus_barcodes("perseus_output_1.txt")
print(h1_barcodes)

# Simple visualization
library(ggplot2)

ggplot(h1_barcodes, aes(x = Birth, xend = Death, y = seq_along(Birth), yend = seq_along(Death))) +
  geom_segment(linewidth = 1.2, color = "blue") +
  labs(title = "H1 Persistent Homology Barcodes", x = "Filtration Parameter", y = "Features") +
  theme_minimal()

# Plot TDA Results (persistent homology barcodes)
plot(tda_result[["diagram"]], main = "Persistent Homology of Grid")

# You may now use this baseline (original state) for comparison after attacks:
save(tda_result, file = "tda_original_state.RData")



end.time = Sys.time()
time.taken = end.time - start.time
