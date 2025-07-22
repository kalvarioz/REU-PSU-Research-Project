suppressPackageStartupMessages({
  library(TDA)
  library(ggplot2)
  
})
setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")

memory.limit(size = 32000)


plot_and_save <- function(diagram, file, title, barcode = TRUE) {
  pdf(file, width = 6, height = 6)
  plot(diagram, barcode = barcode, main = title)
  dev.off()
  message("[saved] ", file)
}

config <- list(
  distance_file_csv    = "environmental_distance_normalizedTEST.csv",
  output_rda           = "tda_result.RData",
  rips_barcode_pdf     = "tda_barcodes.pdf",
  rips_diagram_pdf     = "tda_diagram.pdf",            # new key
  perseus_diagram_png  = "perseus_diagram.png",        # renamed
  backend              = "Dionysus",
  max_dim              = 2,
  max_scale            = 5,
  max_pts              = 100,
  perseus_exe          = normalizePath("Perseus/perseusWin.exe", winslash="\\")
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
main <- function(cfg) {
  D <- as.matrix(read.csv(cfg$distance_file_csv,
                          row.names=1, check.names=FALSE))
  D <- downsample_matrix(D, cfg$max_pts)
  write_perseus_file(D, cfg$max_dim, file="perseus_distmat.txt")
  
  # Rips via TDA
  diag_tda <- run_tda(D, cfg$backend, cfg$max_dim, cfg$max_scale)
  save(diag_tda, file=cfg$output_rda)
  
  # 1) save barcodes
  plot_and_save(diag_tda, cfg$rips_barcode_pdf,
                sprintf("Rips Barcodes (%s)", cfg$backend))
  
  # 2) save persistence diagram
  plot_and_save(diag_tda, "tda_diagram.pdf",
                sprintf("Rips Persistence Diagram (%s)", cfg$backend))
  
  # Perseus on the same data
  perseus_bc <- run_perseus("perseus_distmat.txt",
                            "perseus_output", cfg$perseus_exe)
  
  # scatter-plot of Birth vs Death
  p_diag <- ggplot(perseus_bc, aes(x=Birth, y=Death)) +
    geom_point(size=2) +
    geom_abline(slope=1, intercept=0, linetype="dashed") +
    labs(title="Persistence Diagram via Perseus",
         x="Birth", y="Death") +
    theme_minimal()
  ggsave("perseus_diagram.png", plot=p_diag,
         width=6, height=6)
  
  message("simulation complete")
}

# Run!
main(config)