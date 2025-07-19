suppressPackageStartupMessages({
  library(TDA)
  library(ggplot2)
})
setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")


config <- list(
  distance_file_csv   = "graph_distance_matrix.csv",
  output_rda          = "tda_result.RData",
  rips_barcode_pdf    = "tda_barcodes.pdf",
  perseus_barcode_png = "perseus_barcodes.png",
  backend             = "Dionysus",
  max_dim             = 2,
  max_scale           = 5,
  max_pts             = 100,
  perseus_exe         = normalizePath("Perseus/perseusWin.exe", winslash="\\")
)

downsample_matrix <- function(D, max_pts, seed = 123) {
  M <- nrow(D)
  if (M <= max_pts) return(D)
  set.seed(seed)
  keep <- sample(M, max_pts)
  message("[downsample] ", M, " → ", length(keep), " points")
  D[keep, keep]
}

write_perseus_file <- function(D, max_dim, file = "perseus_distmat.txt") {
  M <- nrow(D); g <- 0; s <- 1
  N <- ceiling(max(D)/s) + 1; C <- max_dim
  vec <- c(M, g, s, N, C, as.vector(D))
  write(vec, file = file, ncolumns = length(vec), sep = " ")
  invisible(file)
}

run_perseus <- function(dist_file, prefix, exe) {
  system2(exe, args = c("distmat", dist_file, prefix),
          stdout = TRUE, stderr = TRUE) |> cat(sep = "\n")
  bc  <- paste0(prefix, "_1.txt")
  if (!file.exists(bc)) stop("Perseus didn’t produce: ", bc)
  read.table(bc, header = FALSE, col.names = c("Birth","Death"))
}

run_tda <- function(D, backend, max_dim, max_scale) {
  ripsDiag(X = D, library = backend,
           maxdimension = max_dim,
           maxscale     = max_scale,
           dist         = "arbitrary",
           printProgress= TRUE)[["diagram"]]
}

plot_and_save <- function(diagram, file, title) {
  pdf(file, width = 8, height = 4)
  plot(diagram, barcode = TRUE, main = title)
  dev.off()
  message("[saved] ", file)
}

main <- function(cfg) {
  D <- as.matrix(read.csv(cfg$distance_file_csv,
                          row.names = 1, check.names = FALSE))
  D <- downsample_matrix(D, cfg$max_pts)
  write_perseus_file(D, cfg$max_dim, file = "perseus_distmat.txt")
  

  diag_tda <- run_tda(D, cfg$backend, cfg$max_dim, cfg$max_scale)
  save(diag_tda, file = cfg$output_rda)
  plot_and_save(diag_tda, cfg$rips_barcode_pdf,
                sprintf("Rips Barcodes (%s)", cfg$backend))
  
  perseus_bc <- run_perseus("perseus_distmat.txt", "perseus_output", cfg$perseus_exe)
  p <- ggplot(perseus_bc, aes(x = Birth, xend = Death,
                              y = seq_along(Birth), yend = seq_along(Death))) +
    geom_segment(size = 1.2, color = "darkgreen") +
    labs(title = "H₁ Barcodes via Perseus",
         x = "Filtration", y = "Feature Index") +
    theme_minimal()
  ggsave(cfg$perseus_barcode_png, plot = p, width = 8, height = 4)
  message("simulation complete")
}

# Run!
main(config)