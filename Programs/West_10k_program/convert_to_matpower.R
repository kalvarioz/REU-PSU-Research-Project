
library(stringr)
library(data.table)

# Main Parsing Function
parse_matpower_to_csv <- function(matpower_file) {
  
  # Read all lines from the .m file
  lines <- readLines(matpower_file)
  
  # Helper function: parses sections
  parse_section <- function(lines, section_name) {
    start_idx <- grep(paste0("^mpc\\.", section_name, " = \\["), lines)
    if (length(start_idx) == 0) stop("Section not found: ", section_name)
    end_idx <- start_idx + 1
    while (!grepl("\\];", lines[end_idx])) end_idx <- end_idx + 1
    
    data_block <- lines[(start_idx + 1):(end_idx - 1)]
    data_clean <- str_squish(data_block)
    data_list <- str_split_fixed(data_clean, "\\s+", n = Inf)
    data_list <- data_list[, colSums(data_list != "") > 0]
    
    df <- as.data.frame(data_list, stringsAsFactors = FALSE)
    
    header_line_idx <- start_idx - 1
    while (!grepl("^%", lines[header_line_idx])) header_line_idx <- header_line_idx - 1
    
    headers <- str_trim(str_remove(lines[header_line_idx], "^%"))
    headers <- unlist(str_split(str_squish(headers), "\\s+"))
  
    colnames(df) <- headers
    
    return(df)
  }
  parse_char_section <-function (lines, section_name){
      start_idx <- grep(paste0("^mpc\\.", section_name, "\\s*=\\s*\\{"), lines)
      if (length(start_idx) == 0) stop("Section not found: ", section_name)
      # Find the first end after the start
      end_idx   <- grep("^\\s*\\};", lines)
      end_idx   <- end_idx[end_idx > start_idx][1]
      if (is.na(end_idx)) stop("Section end not found: ", section_name)
      block     <- lines[(start_idx + 1):(end_idx - 1)]
      # Remove comments, whitespace, quotes, and semicolons
      cellvals  <- gsub("[;'']", "", trimws(gsub("%.*", "", block)))
      cellvals  <- cellvals[cellvals != ""]
      return(cellvals)
  }
  
  # Parse the primary sections
  cat("Parsing bus section...\n")
  bus_df <- parse_section(lines, "bus")
  cat("Parsing generator section...\n")
  gen_df <- parse_section(lines, "gen")
  cat("Parsing branch section...\n")
  branch_df <- parse_section(lines, "branch")
  fuel_types <- parse_char_section(lines, "genfuel")
  bus_names  <- parse_char_section(lines, "bus_name")
  
  write.csv(bus_df,      "mpc_bus.csv",      row.names = FALSE)
  write.csv(gen_df,      "mpc_gen.csv",      row.names = FALSE)
  write.csv(branch_df,   "mpc_branch.csv",   row.names = FALSE)
  write.csv(data.frame(fuel_type=fuel_types), "mpc_genfuel.csv", row.names=FALSE)
  write.csv(data.frame(bus_name=bus_names),   "mpc_bus_name.csv", row.names=FALSE)
}

# Execute the parser function clearly:
# Replace 'case_ACTIVSg10k.m' with your actual filename if different
parse_matpower_to_csv("case_ACTIVSg10k.m")