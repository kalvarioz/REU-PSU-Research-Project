plot_failure_map <- function(area, pts, failed){
  mapview(area, col.regions="lightblue", alpha.regions=.2) +
    mapview(pts, zcol = pts$name %in% failed,
            layer.name="Failed (red) / OK (blue)", burst=TRUE, alpha=.8)
}