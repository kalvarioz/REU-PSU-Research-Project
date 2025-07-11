library(igraph)

aux_file_path <- "ACTIVSg10k.aux"

# explicitly read file lines
lines_from_file <- readLines(aux_file_path)

# inspect the first few lines
print(head(lines_from_file, 10))

# total number of lines read
print(length(lines_from_file))

parse_activsg2000_aux <- function(aux_file_path) {
  lines_from_file <- readLines(aux_file_path)
  lines_from_file <- trimws(lines_from_file)
  
  get_section <- function(start_pattern, lines) {
    start_line <- grep(start_pattern, lines, perl=TRUE)[1]
    if (is.na(start_line)) stop(paste("Section", start_pattern, "not found."))
    next_section <- grep("^DATA \\(", lines[(start_line + 1):length(lines)], perl=TRUE)
    end_line <- if (length(next_section) == 0) length(lines) else (start_line + next_section[1] - 1)
    section_block <- lines[(start_line + 1):(end_line - 1)]
    section_block <- section_block[grepl("^[0-9]+", section_block)]
    return(section_block)
  }
  
  # --- BUS ---
  parse_bus_line <- function(line) {
    # Using regular expressions clearly capturing all fields
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
  
  # --- BRANCH ---
  parse_branch_line <- function(line) {
    # Regular expression to correctly capture quoted or unquoted fields
    pattern <- '("[^"]*"|\\S+)'
    parts <- unlist(regmatches(line, gregexpr(pattern, line))[[1]])
    
    # Remove leading/trailing quotes and spaces from each field
    parts <- gsub('^"|"$', '', trimws(parts))
    
    # Check length, adjust if necessary
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

build_igraph_from_aux <- function(bus_df, branch_df) {
  
  # Match columns exactly
  required_cols <- c("BusNum", "BusNum:1")
  if (!all(required_cols %in% names(branch_df))) {
    stop(paste("Branch dataframe must contain columns:", paste(required_cols, collapse="")))
  }
  
  # Ensure character types for consistency
  branch_df$BusNum <- as.character(branch_df$BusNum)
  branch_df[["BusNum:1"]] <- as.character(branch_df[["BusNum:1"]])
  bus_df$BusNum <- as.character(bus_df$BusNum)
  
  # Create edge list with correct columns
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

aux_data <- parse_activsg2000_aux("ACTIVSg10k.aux")
print(head(aux_data$bus_df))
print(head(aux_data$branch_df))

g2000 <- build_igraph_from_aux(aux_data$bus_df, aux_data$branch_df)
g2000 <- add_environmental_attributes(g2000)
print(summary(g2000))

head(vertex_attr(g2000))
head(edge_attr(g2000))

write.csv(aux_data$bus_df, "10k_buses.csv", row.names = FALSE)
write.csv(aux_data$branch_df, "10k_branches.csv", row.names = FALSE)
write_graph(g2000, "10k_bus_grid.gml", format = "gml")

