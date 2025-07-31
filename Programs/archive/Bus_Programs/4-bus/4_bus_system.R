library(igraph)
library(randomcoloR)

# Load the 4-bus system data
# The data is now read from '4_bus_data.txt' and contains more columns
data <- read.table('4_bus_data.txt', header = TRUE)

# We will create a weighted adjacency matrix based on the 'distance' attribute
# to represent the bus system.
num_buses <- max(max(data$fromBus), max(data$toBus))
eDist <- matrix(Inf, nrow = num_buses, ncol = num_buses)
diag(eDist) <- 0

# Populate the distance matrix with the 'distance' values from the data
for (i in 1:nrow(data)) {
  eDist[data$fromBus[i], data$toBus[i]] <- data$distance[i]
  eDist[data$toBus[i], data$fromBus[i]] <- data$distance[i]
}

# The number of data points 'd' is now the number of buses
d <- num_buses
cap <- 1 # dimension cap
nNghb <- 3 # Number of neighbors to consider for each bus

############################################
# Transforming distance matrix (optional, but kept from original script)
# This step normalizes the distances, which can be useful for TDA
all.dist <- c()
for (i in 1:d) {
  # Find the nearest neighbors for each bus
  ind <- which(rank(eDist[i, ], ties.method = 'first') <= nNghb)
  ind <- setdiff(ind, i)
  all.dist <- c(all.dist, eDist[i, ind])
}

# Using empirical CDF to transform distances to a [0,1] scale
dist.cdf <- ecdf(all.dist)
eDist_transformed <- matrix(dist.cdf(eDist), nrow = d)

############################################

delta <- 0.01 # Step size for filtration
filt_len <- 90 # Filtration length

# Running Perseus
betti_0 <- betti_1 <- c()
for (i in 1:d) {
  # Get the neighborhood for the current bus
  ind <- which(rank(eDist_transformed[i, ], ties.method = 'first') <= nNghb)
  
  # Prepare the distance matrix for the neighborhood to be used by Perseus
  sub_dist <- eDist_transformed[ind, ind]
  
  # Write the data to M.txt for Perseus
  cat(length(ind), file = 'M.txt', append = FALSE, sep = '\n')
  cat(paste(0, delta, filt_len, cap, sep = ' '), file = 'M.txt', append = TRUE, sep = '\n')
  cat(sub_dist, file = 'M.txt', append = TRUE)
  
  # Execute Perseus - you will need the Perseus executable in your working directory
  # system('perseusWin.exe distmat M.txt Moutput') # for Windows
  # system('./perseusMac distmat M.txt Moutput') # for Mac
  # Since we can't run an external executable here, we'll simulate the output for demonstration
  # In a real scenario, you would uncomment the line above.
  # For now, let's create a dummy Moutput_betti.txt
  dummy_betti <- data.frame(
    dim = 0:filt_len,
    b0 = c(length(ind), rep(1, filt_len)),
    b1 = rep(0, filt_len + 1)
  )
  write.table(dummy_betti, 'Moutput_betti.txt', row.names = FALSE, col.names = FALSE)
  
  
  print(i)
  
  betti_data <- as.matrix(read.table('Moutput_betti.txt'))
  
  # The rest of the Betti number processing remains the same
  betti_index <- setdiff(0:filt_len, betti_data[, 1])
  if (length(betti_index) > 0) {
    for (k in betti_index) {
      if (k <= nrow(betti_data)) {
        betti_data <- rbind(betti_data[1:(k - 1), ], betti_data[k - 1, ], betti_data[k:nrow(betti_data), ])
        betti_data[k, 1] <- k
      } else {
        betti_data <- rbind(betti_data, betti_data[nrow(betti_data), ])
        betti_data[nrow(betti_data), 1] <- k
      }
    }
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
  
  # Handle potential division by zero if a norm is 0
  norm_b0 <- norm(as.matrix(betti_0[i, ]), type = 'f')
  norm_b1 <- norm(as.matrix(betti_1[i, ]), type = 'f')
  
  delta_betti_0 <- rbind(delta_betti_0, if (norm_b0 > 0) bettiDist_0[i, ind] / norm_b0 else rep(0, length(ind)))
  delta_betti_1 <- rbind(delta_betti_1, if (norm_b1 > 0) bettiDist_1[i, ind] / norm_b1 else rep(0, length(ind)))
  
  index <- rbind(index, ind)
  
  print(i)
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
for (i in 1:d) {
  index_0 <- which(delta_betti_0[i, ] <= cutoff_0)
  index_1 <- which(delta_betti_1[i, ] <= cutoff_1)
  
  # Make sure there are indices to intersect
  if(length(index_0) > 0 && length(index_1) > 0){
    common_indices <- intersect(index_0, index_1)
    if(length(common_indices) > 0){
      A[i, index[i, common_indices]] <- 1
    }
  }
  print(i)
}

##############################################

# Clustering and Visualization
g <- graph_from_adjacency_matrix(A, mode = 'directed')
clstrs <- clusters(g, mode = 'strong')
distinct.clrs <- distinctColorPalette(clstrs$no)
clrs <- distinct.clrs[clstrs$membership]

# Create a layout for the graph visualization
layout <- layout_with_fr(g)

# Plot the graph of the bus system
plot(g, vertex.color = clrs, vertex.label = 1:num_buses, layout = layout, main = "4-Bus System TDA Clustering")
print(clstrs)