add_dep <- \(src,tgt,dist_m){
  idx <- st_is_within_distance(src, tgt, dist = dist_m)
  do.call(rbind, Map(\(i,js){
    if(!length(js)) return(NULL)
    tibble(from = src$name[i], to = tgt$name[js])
  }, seq_along(idx), idx))
}

dep_e2s <- add_dep(energy_sf,school_sf,5000)
dep_t2s <- add_dep(telecom_sf,school_sf,2000)

supra <- supra |> bind_edges(bind_rows(dep_e2s,dep_t2s) |>
                               mutate(kind="dependency",weight=1))