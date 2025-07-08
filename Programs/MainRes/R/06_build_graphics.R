layers <- list(
  energy  = tbl_graph(nodes = energy_sf,  edges = tibble()),
  telecom = tbl_graph(nodes = telecom_sf, edges = tibble()),
  social  = tbl_graph(nodes = school_sf,  edges = tibble()),
  road    = as_tbl_graph(road_net)
)

supra <- Reduce(\(g1,g2) graph_join(g1,g2, by="name"), layers)