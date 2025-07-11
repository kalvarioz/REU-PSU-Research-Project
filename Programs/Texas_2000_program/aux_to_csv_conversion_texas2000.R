library(igraph)

aux_file_path <- "ACTIVSg2000.aux"

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
  
  # Corrected parsing function
  parse_bus_line <- function(line) {
    matches <- regmatches(line, regexec('^(\\d+)\\s+"([^"]+)"\\s+(\\S+)\\s+"([^"]+)"\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)', line))[[1]]
    
    if (length(matches) >= 17) {
      return(matches[2:17])
    } else {
      return(rep(NA, 16))
    }
  }
  
  bus_block <- get_section("^DATA \\(Bus,", lines_from_file)
  bus_parsed <- t(sapply(bus_block, parse_bus_line))
  
  bus_df <- data.frame(
    BusNum = as.integer(bus_parsed[, 1]),
    BusName = bus_parsed[, 2],
    BusNomVolt = as.numeric(bus_parsed[, 3]),
    BusSlack = bus_parsed[, 4],
    BusPUVolt = as.numeric(bus_parsed[, 7]),
    BusAngle = as.numeric(bus_parsed[, 8]),
    Latitude = as.numeric(bus_parsed[, 15]),
    Longitude = as.numeric(bus_parsed[, 16]),
    stringsAsFactors = FALSE
  )
  
  # BRANCH data remains correctly parsed:
  get_section <- function(start_pattern, lines) {
    start_line <- grep(start_pattern, lines, perl=TRUE)[1]
    if (is.na(start_line)) stop(paste("Section", start_pattern, "not found."))
    
    next_section <- grep("^DATA \\(", lines[(start_line + 1):length(lines)], perl=TRUE)
    end_line <- if (length(next_section) == 0) length(lines) else (start_line + next_section[1] - 1)
    
    section_block <- lines[(start_line + 1):(end_line - 1)]
    # Explicitly filter numeric-starting lines
    section_block <- section_block[grepl("^[0-9]+", section_block)]
    return(section_block)
  }
  
  # Corrected BRANCH Parsing (within parse_activsg2000_aux function):
  branch_block <- get_section("^DATA \\(Branch,", lines_from_file)
  
  parse_branch_line <- function(line) {
    matches <- regmatches(line, regexec('^(\\d+)\\s+(\\d+)\\s+"([^"]+)"\\s+"([^"]+)"\\s+"[^"]*"\\s+"([^"]+)"', line))[[1]]
    if (length(matches) >= 6) {
      return(matches[2:6])
    } else {
      return(rep(NA, 5))
    }
  }

  branch_parsed <- t(sapply(branch_block, parse_branch_line))
  
  branch_df <- data.frame(
    FromBus = as.integer(branch_parsed[, 1]),
    ToBus = as.integer(branch_parsed[, 2]),
    Circuit = branch_parsed[, 3],
    BranchDeviceType = branch_parsed[, 4],
    LineStatus = branch_parsed[, 5],
    stringsAsFactors = FALSE
  )
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
  edge_list <- data.frame(
    from = as.character(branch_df$FromBus),
    to = as.character(branch_df$ToBus),
    label = paste0("F_", seq_len(nrow(branch_df)))
  )
  
  bus_df$BusNum <- as.character(bus_df$BusNum)
  g <- graph_from_data_frame(edge_list, directed = TRUE, vertices = bus_df)
  V(g)$name <- bus_df$BusNum
  return(g)
}
aux_data <- parse_activsg2000_aux("ACTIVSg2000.aux")
print(head(aux_data$bus_df))
print(head(aux_data$branch_df))

g2000 <- build_igraph_from_aux(aux_data$bus_df, aux_data$branch_df)
g2000 <- add_environmental_attributes(g2000)
print(summary(g2000))

# Verify vertex and edge attributes
head(vertex_attr(g2000))
head(edge_attr(g2000))

write.csv(result2000_df, "result-2000.csv", row.names = FALSE)
write_graph(g2000, "2000_bus_grid.gml", format = "gml")
