# 0) Throttle resources (Windows-only memory limit + thread caps)
if (.Platform$OS.type == "windows") {
  memory.limit(size = 32000)  
  message("R memory limited to 32 GB on Windows")
}
phys_cores <- parallel::detectCores(logical = FALSE)
Sys.setenv(
  OMP_NUM_THREADS      = phys_cores,
  OPENBLAS_NUM_THREADS = phys_cores,
  MKL_NUM_THREADS      = phys_cores
)
message(sprintf("[config] OMP threads set to %d", phys_cores))

# 1) Libraries
suppressPackageStartupMessages({
  library(data.table)    # fast CSV I/O
  library(bigmemory)     # memory‐mapped matrices
  library(TDA)       
  library(ripserr)   
  library(TDAstats)  
  library(ggplot2)
})

# 2) Configuration
config <- list(
  distance_file_csv   = "net_power_difference_normalizedTEST.csv",
  output_rda          = "tda_result.RData",
  rips_barcode_pdf    = "tda_barcodes.pdf",
  rips_diagram_pdf    = "tda_diagram.pdf",
  perseus_diagram_png = "perseus_diagram.png",
  backend             = "ripserr",    # "ripserr", "TDAstats" or "Dionysus"
  max_dim             = 2,
  max_scale           = 1,           
  perseus_exe         = normalizePath("Perseus/perseusWin.exe",
                                      winslash="\\")
)

# 3) Helper functions

downsample_matrix <- function(D, max_pts, seed = 123) {
  M <- nrow(D)
  if (is.null(max_pts) || M <= max_pts) return(D)
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
}

run_perseus <- function(dist_file, prefix, exe) {
  system2(exe, args = c("distmat", dist_file, prefix),
          stdout = TRUE, stderr = TRUE) |> cat(sep = "\n")
  bc <- paste0(prefix, "_1.txt")
  if (!file.exists(bc)) stop("Perseus didn’t produce: ", bc)
  read.table(bc, header = FALSE, col.names = c("Birth","Death"))
}

run_tda <- function(D, backend, max_dim, max_scale) {
  switch(backend,
         Dionysus = {
           dg <- ripsDiag(
             X      = D,
             dist   = "arbitrary",
             library="Dionysus",
             maxdimension = max_dim,
             maxscale     = max_scale,
             printProgress= TRUE
           )$diagram
           as.data.frame(setNames(dg, c("Dimension","Birth","Death")))
         },
         
         ripserr = {
           # convert your full distance matrix to a 'dist' object
           Dd   <- as.dist(D)
           # call the exported function
           phom <- ripserr::vietoris_rips(
             Dd,
             max_dim   = max_dim,
             threshold = max_scale
           )
           # turn it into the same data.frame format
           df <- as.data.frame(phom)
           setNames(df, c("Dimension","Birth","Death"))
         },
         
         TDAstats = {
           phom <- TDAstats::calculate_homology(
             D,
             dim       = max_dim,
             threshold = max_scale,
             format    = "distmat"
           )
           df <- as.data.frame(phom)
           setNames(df, c("Dimension","Birth","Death"))
         },
         
         stop("Unknown backend: ", backend)
  )
}

plot_and_save <- function(diagram, file, title, barcode = TRUE) {
  pdf(file, width = 6, height = 6)
  plot(diagram, barcode = barcode, main = title)
  dev.off()
  message("[saved] ", file)
}

# 4) Main pipeline

main <- function(cfg) {
  message("[step] Reading CSV via data.table::fread()")
  Ddt <- fread(cfg$distance_file_csv)
  message(sprintf("[done] fread: %dx%d, object size %s",
                  nrow(Ddt), ncol(Ddt),
                  format(object.size(Ddt), units = "auto")))
  
  # Explicitly remove non-numeric first column (row IDs):
  message("[step] Stripping ID column (‘BusNum’) and coercing to numeric matrix")
  # Drop the first column entirely, whatever its name is:
  mat <- as.matrix(Ddt[, -1, with = FALSE])
  # (Optional) restore rownames if you need them downstream
  rownames(mat) <- as.character(Ddt[[1]])
  
  # Validate numeric:
  if (!is.numeric(mat)) {
    stop("Error: non-numeric data detected in 'mat' after dropping IDs.")
  }
  rm(Ddt); gc()
  
  message("[step] Creating big.matrix (memory-mapped)")
  Dm <- as.big.matrix(
    mat,
    backingfile    = "D.bin",
    descriptorfile = "D.desc",
    type           = "double"
  )
  rm(mat); gc()
  
  message("[done] big.matrix created:",
          " backingfile = D.bin;",
          " descriptorfile = D.desc")

  
  # ---- RUN TDA IN R ----
  message("[step] Computing homology with backend: ", cfg$backend)
  full_mat <- as.matrix(Dm[, ])
  diag_tda <- run_tda(full_mat, cfg$backend, cfg$max_dim, cfg$max_scale)
  save(diag_tda, file = cfg$output_rda)
  message("[done] Saved R-generated diagram to RData")
  
  # ---- PLOT R DIAGRAMS ----
  plot_and_save(diag_tda, cfg$rips_barcode_pdf, "Rips Barcodes", TRUE)
  plot_and_save(diag_tda, cfg$rips_diagram_pdf, "Rips Diagram", FALSE)
  
  # ----- WRITE & STREAM TO PERSEUS CLI -----
  message("[step] Writing Perseus input & streaming to CLI")
  write_perseus_file(full_mat, cfg$max_dim, file = "perseus_distmat.txt")
  system2(cfg$perseus_exe,
          args = c("distmat", "perseus_distmat.txt", "perseus_out"),
          stdout = function(x) message(x),
          stderr = function(x) message(x))
  message("[done] Perseus CLI complete; reading output")
  
  perseus_bc <- read.table("perseus_out_1.txt",
                           header    = FALSE,
                           col.names = c("Birth","Death"))
  
  message("[step] Plotting Perseus output via ggplot2")
  p_diag <- ggplot(perseus_bc, aes(Birth, Death)) +
    geom_point(size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = "Perseus Persistence Diagram", x = "Birth", y = "Death") +
    theme_minimal()
  ggsave(cfg$perseus_diagram_png, plot = p_diag, width = 6, height = 6)
  message("[done] Perseus diagram saved to ", cfg$perseus_diagram_png)
  
  message("[complete] TDA pipeline finished")
}

# 5) Run it!
main(config)