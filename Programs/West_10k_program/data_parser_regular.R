library(stringr)


schemas <- list(
  Bus    = c("BusNum", "BusName", "BusNomVolt", "BusSlack", 
             "Bus:B1", "BusG:1", "BusPUVolt", "BusAngle",
             "DCLossMultiplier", "AreaNum", "ZoneNum", "BANumber", 
             "OwnerNum", "SubNum", "Latitude", "Longitude"),
  Branch = c("BusNum", "BusNum.1", "LineCircuit", "BranchDeviceType", 
             "ConsolidateBranch", "LineStatus", "NormLineStatus", 
             "SeriesCapStatus", "LineMeter", "LineR", "LineX",
             "LineC", "LineG", "LineLength", "LineMonEle"),
  Gen    = c("BusNum","GenID","GenStatus","GenVoltSet","GenRegNum","GenRMPCT","GenAGCAble","GenParFac",
             "GenMWSetPoint","GenMWMax","GenMWMin","GenEnforceMWLimits","GenAVRAble","GenMvrSetPoint",
             "GenMVRMax","GenMVRMin","GenUseCapCurve","GenWindControlMode","GenWindPowerFactor",
             "GenUseLDCRCC","GenRLDCRCC","GenXLDCRCC","GenMVABase","GenZR","GenZX","GenStepR","GenStepX",
             "GenStepTap","TSGovRespLimit","GenUnitType:1","AreaNum","ZoneNum","BANumber","OwnerNum",
             "OwnPercent","OwnerNum:1","OwnPercent:1","OwnerNum:2","OwnPercent:2","OwnerNum:3",
             "OwnPercent:3","OwnerNum:4","OwnPercent:4","OwnerNum:5","OwnPercent:5","OwnerNum:6",
             "OwnPercent:6","OwnerNum:7","OwnPercent:7","EMSType","EMSDeviceID","DataMaintainerAssign",
             "llLabels","GenUnitType","GenTotalFixedCosts","GenCostModel","GenFuelType","GenFuelCost",
             "GenFixedCost","GenIOD","GenIOC","GenIOB"),
  Load   =  c("BusNum","LoadID","LoadStatus","GenAGCAble","LoadSMW","LoadSMVR","LoadIMW",
              "LoadIMVR","LoadZMW","LoadZMVR","DistStatus","DistMWInput","DistMvarInput",
              "Interruptable","GenMWMax","GenMWMin","LoadModelGroup","AreaNum","ZoneNum",
              "BANumber","OwnerNum","EMSType","EMSDeviceID","DataMaintainerAssign","AllLabels")
  )


get_section <- function(start_pattern, lines) {
  start_line <- grep(start_pattern, lines, perl = TRUE)[1]
  if (is.na(start_line)) stop(paste("Section", start_pattern, "not found."))
  next_section <- grep("^DATA \\(", lines[(start_line + 1):length(lines)], perl = TRUE)
  end_line <- if (length(next_section) == 0) length(lines) else (start_line + next_section[1] - 1)
  block <- lines[(start_line + 1):(end_line - 1)]
  block <- block[grepl("^[0-9]+", block)]
  return(block)
}

parse_section_generic <- function(lines, section_name, field_names) {
  block <- get_section(paste0("^DATA \\(", section_name, ","), lines)
  
  tokens <- lapply(block, function(line) {
    parts <- regmatches(line, gregexpr('("[^"]*"|\\S+)', line, perl=TRUE))[[1]]
    parts <- gsub('^"|"$', '', parts)
    length(parts) <- length(field_names)
    parts
  })
  
  df <- as.data.frame(do.call(rbind, tokens), stringsAsFactors=FALSE)
  colnames(df) <- field_names
  
  
  
  # Automatic numeric conversion where possible
  is_num <- sapply(df[1,], function(x) grepl('^-?[0-9]*\\.?[0-9]+$', x))
  df[,is_num] <- lapply(df[,is_num], as.numeric)
  
  return(df)
}

# ---- Main Parser Function ----

parse_activsg_aux <- function(aux_file_path, output_dir = ".") {
  lines <- trimws(readLines(aux_file_path))
  results <- list()
  
  for (sec in names(schemas)) {
    cat("Parsing", sec, "section...\n")
    df <- parse_section_generic(lines, sec, schemas[[sec]])
    results[[sec]] <- df
    write.csv(df, file.path(output_dir, paste0(tolower(sec), "_data.csv")), row.names = FALSE)
  }
  
  cat("Parsing completed. CSV files created in", output_dir, "\n")
  return(results)
}
parsed_data <- parse_activsg_aux("ACTIVSg10k.aux", output_dir = "parsed_regular_csv")