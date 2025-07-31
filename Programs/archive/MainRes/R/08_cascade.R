cascade <- function(G, seeds, order_k = 5){
  g <- as.igraph(G)
  failed <- seeds
  for(k in seq_len(order_k)){
    nbrs <- unlist(neighborhood(g,1,failed,mode="all"))
    failed <- unique(c(failed,nbrs))
  }
  failed
}

haz_poly <- st_read(haz_flood_gj, quiet=TRUE) |> st_transform(4326)
seed_nodes <- points_sf$name[st_intersects(points_sf,haz_poly,sparse=FALSE)[,1]]
failed_all <- cascade(supra, seed_nodes, 5)
cat(length(seed_nodes),"seed →",length(failed_all),"failed\n")