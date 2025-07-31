library(igraph)
library(randomcoloR)

# Load the combined dataset, specifying the file encoding to handle files saved from Excel
data <- read.table('Datasets_for_4buses.txt', header = TRUE, fileEncoding = "UTF-16LE")

# The data for distance calculation are the x and y coordinates
spatial_data <- data[, c("x", "y")]

cap <- 1 # dimension cap
d <- nrow(spatial_data) # number of points (original + buses)

# Compute Euclidean distance between all points
eDist <- as.matrix(dist(spatial_data))
nNghb <- 10 # number of neighbors around each data point (can be tuned)

############################################
# Transforming distance matrix (optional step from original script)
all.dist <- c()
for (i in 1:d) {
  ind <- which(rank(eDist[i, ], ties.method = 'first') <= nNghb)
  ind <- setdiff(ind, i)
  all.dist <- c(all.dist, eDist[i, ind])
}
dist.cdf <- ecdf(all.dist)
eDist_transformed <- matrix(dist.cdf(eDist), nrow = d)

############################################

delta <- 0.01 # step size for filtration
filt_len <- 90 # filtration length

# Running Perseus
betti_0 <- betti_1 <- c()
for (i in 1:d) {
  ind <- which(rank(eDist_transformed[i, ], ties.method = 'first') <= nNghb)
  sub_dist <- eDist_transformed[ind, ind]
  
  cat(length(ind), file = 'M.txt', append = FALSE, sep = '\n')
  cat(paste(0, delta, filt_len, cap, sep = ' '), file = 'M.txt', append = TRUE, sep = '\n')
  cat(sub_dist, file = 'M.txt', append = TRUE)
  
  # --- Perseus Execution Placeholder ---
  # In a real environment, you would run Perseus here.
  # system('perseusWin.exe distmat M.txt Moutput')
  # For this example, we simulate the output.
  dummy_betti <- data.frame(
    dim = 0:filt_len,
    b0 = c(length(ind), rep(1, filt_len)),
    b1 = rep(0, filt_len + 1)
  )
  write.table(dummy_betti, 'Moutput_betti.txt', row.names = FALSE, col.names = FALSE)
  # --- End Placeholder ---
  
  print(paste("Processing point", i))
  
  betti_data <- as.matrix(read.table('Moutput_betti.txt'))
  
  # Fill in omitted Betti numbers
  betti_index <- setdiff(0:filt_len, betti_data[, 1])
  if (length(betti_index) > 0) {
    betti_data_filled <- matrix(0, nrow = filt_len + 1, ncol = ncol(betti_data))
    betti_data_filled[,1] <- 0:filt_len
    betti_data_filled[betti_data[,1] + 1, -1] <- betti_data[,-1]
    
    # Simple forward fill for missing values
    for(k in 2:nrow(betti_data_filled)){
      if(sum(betti_data_filled[k, -1]) == 0){
        betti_data_filled[k,-1] = betti_data_filled[k-1,-1]
      }
    }
    betti_data <- betti_data_filled
  }
  
  betti_0 <- rbind(betti_0, betti_data[, 2])
  betti_1 <- rbind(betti_1, betti_data[, 3])
}
##################################################

bettiDist_0 <- as.matrix(dist(betti_0))
bettiDist_1 <- as.matrix(dist(betti_1))

##################################################

# Computing relative change in Betti numbers
delta_betti_0 <- delta_betti_1 <- c()
index <- c()
for (i in 1:d) {
  ind <- which(rank(eDist_transformed[i, ], ties.method = 'first') <= nNghb)
  ind <- ind[order(eDist_transformed[i, ind])]
  ind <- setdiff(ind, i)
  
  norm_b0 <- norm(as.matrix(betti_0[i, ]), type = 'f')
  norm_b1 <- norm(as.matrix(betti_1[i, ]), type = 'f')
  
  if(length(ind) > 0){
    delta_betti_0 <- rbind(delta_betti_0, if (norm_b0 > 0) bettiDist_0[i, ind] / norm_b0 else rep(0, length(ind)))
    delta_betti_1 <- rbind(delta_betti_1, if (norm_b1 > 0) bettiDist_1[i, ind] / norm_b1 else rep(0, length(ind)))
    index <- rbind(index, ind)
  }
  
  print(paste("Calculating relative change for point", i))
}

##################################################

par(mfrow = c(1, 2))
bp_0 <- boxplot(as.vector(delta_betti_0), pch = 20, ylab = 'Relative change in Betti-0')
bp_1 <- boxplot(as.vector(delta_betti_1), pch = 20, ylab = 'Relative change in Betti-1')
par(mfrow = c(1, 1))

cutoff_0 <- bp_0$stats[5, ]
cutoff_1 <- bp_1$stats[5, ]

##############################################

# Forming adjacency matrix
A <- matrix(0, ncol = d, nrow = d)
for (i in 1:nrow(index))
{
  index_0 <- which(delta_betti_0[i, ] <= cutoff_0)
  index_1 <- which(delta_betti_1[i, ] <= cutoff_1)
  
  if (length(index_0) > 0 && length(index_1) > 0) {
    common_indices <- intersect(index_0, index_1)
    if (length(common_indices) > 0) {
      A[i, index[i, common_indices]] <- 1
    }
  }
  print(paste("Forming adjacency matrix for point", i))
}

##############################################

# Clustering and Visualization
g <- graph_from_adjacency_matrix(A, mode = 'directed')
clstrs <- clusters(g, mode = 'strong')

##############################################
#
# NEW SECTION: EXPORT TO POWERWORLD .AUX FORMAT (8-Bus Style)
#
##############################################

# Define the output file name
output_aux_file <- "4bus_system.aux"

# Start with a clean file
cat("", file = output_aux_file)

# --- Write Header and SCRIPT block ---
cat("//--------------------------------------------------------------------------------\n", file = output_aux_file, append = TRUE)
cat("// PowerWorld Simulator AUX File\n", file = output_aux_file, append = TRUE)
cat("// Created from TDA Clustering R Script\n", file = output_aux_file, append = TRUE)
cat("// Defines a 4-bus case with TDA cluster information\n", file = output_aux_file, append = TRUE)
cat("//--------------------------------------------------------------------------------\n\n", file = output_aux_file, append = TRUE)
cat("SCRIPT\n{\nEnterMode(Edit);\n}\n\n", file = output_aux_file, append = TRUE)

# --- 1. Export Bus Data ---

# Isolate the bus data and their cluster memberships
bus_indices <- which(data$type == "bus")
bus_clusters <- clstrs$membership[bus_indices]
num_buses_in_system <- length(bus_indices)

# Write Bus Header
cat("Bus (Number,Name,NomkV,Slack)\n{\n", file = output_aux_file, append = TRUE)

# Write each bus record
for (i in 1:num_buses_in_system) {
  bus_num <- i
  # We'll embed the TDA cluster group into the bus name
  bus_name <- paste0("\"Bus ", bus_num, " - Grp ", bus_clusters[i], "\"") 
  bus_kv <- 138.0
  # Designate the first bus as the slack bus
  is_slack <- ifelse(i == 1, "\"YES\"", "\"NO \"") 
  
  cat(paste("    ", bus_num, bus_name, bus_kv, is_slack, "\n"), file = output_aux_file, append = TRUE)
}
cat("}\n\n", file = output_aux_file, append = TRUE)


# --- 2. Export Generator Data ---
# We will add one generator at the slack bus (Bus 1) to make the case solvable.
cat("Gen (BusNum,ID,Status,MWSetPoint,MWMax,MWMin)\n{\n", file = output_aux_file, append = TRUE)
cat("    1 \"1\" \"Closed\" 100.0 500.0 0.0\n", file = output_aux_file, append = TRUE)
cat("}\n\n", file = output_aux_file, append = TRUE)


# --- 3. Export Branch Data ---

# Make sure you have a 'bus_data.txt' file in your directory with branch info
# fromBus toBus rateA rateB rateC distance
branch_info <- read.table('4_bus_data.txt', header = TRUE)

# Write Branch Header
cat("Branch (BusNumFrom,BusNumTo,Circuit,R,X,LimitMVAA,LimitMVAB,LimitMVAC)\n{\n", file = output_aux_file, append = TRUE)

# Write each branch record
for (i in 1:nrow(branch_info)) {
  from_bus <- branch_info$fromBus[i]
  to_bus <- branch_info$toBus[i]
  ckt <- "\"1\""
  # Assuming X = 0.001 per unit distance and R = X / 10
  r_val <- (branch_info$distance[i] * 0.001) / 10
  x_val <- branch_info$distance[i] * 0.001
  rate_a <- branch_info$rateA[i]
  rate_b <- branch_info$rateB[i]
  rate_c <- branch_info$rateC[i]
  
  cat(paste("    ", from_bus, to_bus, ckt, r_val, x_val, rate_a, rate_b, rate_c, "\n"), file = output_aux_file, append = TRUE)
}
cat("}\n\n", file = output_aux_file, append = TRUE)

# --- Write Footer SCRIPT block ---
cat("SCRIPT\n{\nClearPowerFlowSolutionAidValues;\n}\n", file = output_aux_file, append = TRUE)

cat(paste("PowerWorld case data written to", output_aux_file, "\n"))


##############################################
# Original Visualization (kept for reference)
distinct.clrs <- distinctColorPalette(clstrs$no)
clrs <- distinct.clrs[clstrs$membership]
point_shapes <- ifelse(data$type == "bus", 17, 20)
plot(data$x, data$y, col = clrs, pch = point_shapes,
     main = "TDA Clustering of Combined Data",
     xlab = "X Coordinate", ylab = "Y Coordinate")
legend("topright", legend = c("Original Point", "Bus"), pch = c(20, 17))
print(clstrs)
