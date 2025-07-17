library(shiny)
library(leaflet)
library(sf)
library(igraph)
library(dplyr)
library(maps)
library(ggplot2)
library(foreach)
library(doParallel)
library(future.apply)
library(future)

setwd("~/REU-PSU-Research-Project/Programs/West_10k_program")

# ──────────────────────────────────────────────────────────────────────────────
#  1. DATA LOADING & PREP
# ──────────────────────────────────────────────────────────────────────────────

# 1.a MATPOWER CSV exports
plan(multisession, workers = detectCores() - 1)

mpc_bus    <- read.csv("mpc_bus.csv",      stringsAsFactors = FALSE)
mpc_gen    <- read.csv("mpc_gen.csv",      stringsAsFactors = FALSE)
bus_names  <- read.csv("mpc_bus_name.csv", stringsAsFactors = FALSE)
fuel_types <- read.csv("mpc_genfuel.csv",  stringsAsFactors = FALSE)

mpc_bus$bus_name  <- bus_names$bus_name
mpc_gen$fuel_type <- fuel_types$fuel_type

# 1.b Pre-attack metrics
bus_load_pre <- mpc_bus %>%
  select(bus_i, Pd, Vm, bus_name)

bus_gen_pre <- mpc_gen %>%
  group_by(bus) %>%
  summarise(
    total_gen    = sum(Pg,    na.rm = TRUE),
    max_capacity = sum(Pmax,  na.rm = TRUE),
    fuel_type    = paste(unique(fuel_type), collapse = "/")
  ) %>%
  rename(bus_i = bus)

bus_info_pre <- bus_load_pre %>%
  left_join(bus_gen_pre, by = "bus_i") %>%
  mutate(
    total_gen      = ifelse(is.na(total_gen), 0, total_gen),
    gen_load_ratio = total_gen / (Pd + 1e-3),
    bus_type       = case_when(
      total_gen > 0 & Pd > 0 ~ "Gen + Load",
      total_gen > 0          ~ "Generator",
      Pd       > 0           ~ "Load",
      TRUE                   ~ "Neither"
    )
  )

# 1.c Geospatial & graph data
bus_data    <- read.csv("10k_buses.csv",    stringsAsFactors = FALSE)
branch_data <- read.csv("10k_branches.csv", stringsAsFactors = FALSE)%>%
  mutate(
    BusNum   = as.integer(BusNum),
    BusNum.1 = as.integer(BusNum.1)
  ) %>%
  # define unordered keys
  mutate(
    b1 = pmin(BusNum, BusNum.1),
    b2 = pmax(BusNum, BusNum.1)
  ) %>%
  # keep exactly one row per undirected line
  distinct(b1, b2, .keep_all = TRUE)


graph_original <- read_graph("10k_bus_grid.gml", format = "gml")
graph_original <- as.undirected(graph_original, mode = "collapse")

# extract edge list
ed2 <- igraph::as_data_frame(graph_original, what = "edges") %>%
  # make your join keys
  transmute(
    from_bus = as.integer(from),
    to_bus   = as.integer(to)
  ) %>%
  mutate(
    b1 = pmin(from_bus, to_bus),
    b2 = pmax(from_bus, to_bus)
  ) %>%
  # join on the deduped branch_data
  left_join(
    branch_data %>% select(b1, b2, LineR, LineX),
    by = c("b1", "b2")
  )

# now ed2 has exactly one row per undirected edge
# write back into the graph
E(graph_original)$LineR <- ed2$LineR
E(graph_original)$LineX <- ed2$LineX


# 4) Now you can do Y = 1/(R + 1i*X)
buses_sf <- st_as_sf(bus_data,
                     coords = c("Longitude.1","Latitude.1"),
                     crs    = 4326, remove = FALSE)

states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
west_states <- c("california","oregon","washington","idaho","montana",
                 "wyoming","nevada","utah","arizona","colorado","new mexico")
west_sf <- states_sf[states_sf$ID %in% west_states, ]

# 1.d Build graph + relationship matrices           ### UPDATED ###

# — Compute true line admittances (Y = 1/(R + jX)) —
#    (replace R / X with your actual attribute names if they differ)
E(graph_original)$Y <- 1 / (E(graph_original)$LineR + 1i * E(graph_original)$LineX)
E(graph_original)$Yreal <- Re(E(graph_original)$Y)
# — Pull weighted adjacency from the new Y attribute —
W_full <- as.matrix(
  as_adjacency_matrix(
    graph_original,
    attr   = "Yreal",    # or "Ymag"
    sparse = FALSE
  )
)


# Per-node strength & add into bus_info_pre
node_strength_full <- rowSums(W_full)
bus_info_pre <- bus_info_pre %>%
  mutate(strength = node_strength_full[ as.character(bus_i) ])

# Global Laplacian & algebraic connectivity
L_full  <- diag(rowSums(W_full)) - W_full
eig_full <- eigen(L_full, symmetric = TRUE, only.values = TRUE)$values
algebraic_connectivity_full <- eig_full[2]

# ──────────────────────────────────────────────────────────────────────────────
#  2. COLOR PALETTES
# ──────────────────────────────────────────────────────────────────────────────

bus_pal <- colorFactor(
  palette = c(
    "Generator"  = "#e41a1c",
    "Load"       = "#377eb8",
    "Gen + Load" = "#4daf4a",
    "Neither"    = "#999999"
  ), domain = bus_info_pre$bus_type
)

fuel_pal <- colorFactor(
  palette = c(
    hydro   = "#1f78b4",
    wind    = "#a6cee3",
    solar   = "#feb24c",
    ng      = "#fc8d59",
    coal    = "#636363",
    nuclear = "#756bb1",
    UN      = "#b2df8a"
  ), domain = unique(mpc_gen$fuel_type)
)

# ──────────────────────────────────────────────────────────────────────────────
#  3. HELPER FUNCTIONS
# ──────────────────────────────────────────────────────────────────────────────
recompute_edge_attrs <- function(g, branch_data) {
  # 1) pull current edge list
  el <- igraph::as_data_frame(g, what = "edges") %>%
    transmute(
      from = as.integer(from), to = as.integer(to),
      b1 = pmin(from, to), b2 = pmax(from, to)
    )
  # 2) bring back LineR/LineX from your master table
  joined <- left_join(el, branch_data %>% select(b1, b2, LineR, LineX),
                      by = c("b1","b2"))
  # 3) sanity check
  if (nrow(joined) != igraph::ecount(g))
    stop("Edge count mismatch – cannot recompute attributes reliably")
  # 4) assign them all at once
  E(g)$LineR <- joined$LineR
  E(g)$LineX <- joined$LineX
  E(g)$Y     <- 1/(joined$LineR + 1i*joined$LineX)
  E(g)$Yreal <- Re(E(g)$Y)
  g
}

simulate_attack <- function(g, buses_to_remove = NULL) {
  if (!is.null(buses_to_remove)) {
    valid <- intersect(as.character(buses_to_remove), V(g)$name)
    g <- delete_vertices(g, valid)
  }
  g <- simplify(g, remove.multiple=TRUE, remove.loops=TRUE)
  if (ecount(g) > 0 && !is.null(E(g)$LineR) && !is.null(E(g)$LineX)) {
    E(g)$Yreal <- Re(1 / (E(g)$LineR + 1i * E(g)$LineX))
  }
  g <- recompute_edge_attrs(g, branch_data)
  g
}

simulate_fire_cascade <- function(graph, buses_sf, fire_polys, steps=20) {
  graphs <- list(graph)
  buses_lost_per_step <- list()
  
  for (i in seq_len(min(steps, length(fire_polys)))) {
    perim <- fire_polys[[i]]
    hits  <- st_within(buses_sf, perim)
    to_rm <- buses_sf$BusNum[lengths(hits)>0]
    
    g_new <- simulate_attack(graphs[[i]], buses_to_remove=to_rm)
    comps <- components(g_new)
    gens  <- as.character(bus_info_pre$bus_i[bus_info_pre$total_gen>0])
    deenergized <- unlist(lapply(seq_along(comps$csize), function(idx) {
      nodes <- V(g_new)$name[comps$membership==idx]
      if (!any(nodes %in% gens)) nodes else NULL
    }))
    valid_de <- intersect(deenergized, V(g_new)$name)
    g_new <- delete_vertices(g_new, valid_de)
    g_new <- simplify(g_new, remove.multiple=TRUE, remove.loops=TRUE)
    
    # FIX 4: Recompute edge weights after topology change
    if (ecount(g_new) > 0 && !is.null(E(g_new)$LineR) && !is.null(E(g_new)$LineX)) {
      E(g_new)$Yreal <- Re(1 / (E(g_new)$LineR + 1i * E(g_new)$LineX))
    }
    
    g_new <- recompute_edge_attrs(g_new, branch_data)
    
    graphs[[i+1]]         <- g_new
    buses_lost_per_step[[i+1]] <- as.numeric(c(to_rm, valid_de))
  }
  
  list(graphs = graphs, buses_lost_per_step = buses_lost_per_step)
}



prepare_edges_sf <- function(graph, bus_data) {
  bd <- bus_data %>% mutate(BusNum = as.character(BusNum))
  edges_df <- igraph::get.data.frame(graph, what = "edges") %>%
    mutate(from = as.character(from), to = as.character(to)) %>%
    left_join(bd, by = c("from" = "BusNum")) %>%
    rename(lon_from = Longitude.1, lat_from = Latitude.1) %>%
    left_join(bd, by = c("to"   = "BusNum")) %>%
    rename(lon_to   = Longitude.1, lat_to   = Latitude.1) %>%
    filter(!is.na(lon_from), !is.na(lat_from),
           !is.na(lon_to),   !is.na(lat_to))
  
  lines_list <- lapply(seq_len(nrow(edges_df)), function(i) {
    st_linestring(matrix(
      c(edges_df$lon_from[i], edges_df$lat_from[i],
        edges_df$lon_to[i],   edges_df$lat_to[i]),
      ncol = 2, byrow = TRUE
    ))
  })
  st_sfc(lines_list, crs = 4326)
}

# ──────────────────────────────────────────────────────────────────────────────
#  4. FIRE SPREAD MODEL SETUP
# ──────────────────────────────────────────────────────────────────────────────

set.seed(123)
steps      <- 20
center_lon <- -122.4194
center_lat <-  37.7749

neighbors <- as_adj_list(graph_original, mode = "all")
names(neighbors) <- V(graph_original)$name

spread_fire <- function(initial, p_spread = 0.3) {
  on_fire <- initial; newly <- initial
  while (length(newly) > 0) {
    next_new <- integer()
    for (b in newly) {
      b_chr <- as.character(b)
      if (!(b_chr %in% names(neighbors))) next
      for (nbr in neighbors[[b_chr]]) {
        if (!(nbr %in% on_fire) && runif(1) < p_spread) {
          next_new <- c(next_new, as.integer(nbr))
        }
      }
    }
    newly   <- unique(next_new)
    on_fire <- unique(c(on_fire, newly))
  }
  on_fire
}

initial_hotspots <- buses_sf %>%
  filter(abs(Longitude.1 - center_lon) < 0.1,
         abs(Latitude.1  - center_lat)  < 0.1) %>%
  slice_sample(n = 5) %>% pull(BusNum)

on_fire_steps <- vector("list", steps)
on_fire_steps[[1]] <- initial_hotspots
for (t in 2:steps) {
  on_fire_steps[[t]] <- spread_fire(on_fire_steps[[t-1]], p_spread = 0.3)
}

fire_points_list  <- lapply(on_fire_steps, function(ids) buses_sf %>% filter(BusNum %in% ids))
burned_area_list  <- lapply(fire_points_list, function(pt) {
  if (nrow(pt)>0) st_union(st_buffer(pt, dist=2000)) else st_sfc()
})
synthetic_perims <- burned_area_list

cascade_result      <- simulate_fire_cascade(graph_original, buses_sf, synthetic_perims, steps)
graph_sequence      <- cascade_result$graphs
buses_lost_per_step <- cascade_result$buses_lost_per_step

# ──────────────────────────────────────────────────────────────────────────────
#  5. RESILIENCE METRICS (with spectral)
# ──────────────────────────────────────────────────────────────────────────────

resilience_metrics_df <- future_lapply(0:steps, function(step) {
  g      <- graph_sequence[[step+1]]
  comps  <- components(g)
  active <- as.numeric(V(g)$name)
  
  # power metrics
  post        <- bus_info_pre %>% mutate(is_active = bus_i %in% active)
  total_load  <- sum(post$Pd)
  total_gen   <- sum(post$total_gen)
  load_served <- sum(post$Pd[post$is_active])
  gen_served  <- sum(post$total_gen[post$is_active])
  
  if (is.null(E(g)$Yreal) && ecount(g) > 0) {
    E(g)$Yreal <- Re(1 / (E(g)$LineR + 1i * E(g)$LineX))
  }
  # spectral metrics
  W_cur <- as.matrix(
    as_adjacency_matrix(
      g,
      attr   = "Yreal",   # ← now numeric
      sparse = FALSE
    )
  )
  strength_cur <- rowSums(W_cur)
  avg_str      <- mean(strength_cur)
  
  # — Spectral metrics guaranteed real —
  L_cur   <- diag(rowSums(W_cur)) - W_cur
  eig_cur <- eigen(L_cur, symmetric = TRUE, only.values = TRUE)$values  ### UPDATED ###
  lam2    <- eig_cur[2]
  
  
  data.frame(
    step                   = step,
    islands                = comps$no,
    largest_island         = max(comps$csize),
    load_served_pct        = 100 * load_served / total_load,
    gen_served_pct         = 100 * gen_served  / total_gen,
    buses_active           = length(active),
    buses_lost             = nrow(bus_info_pre) - length(active),
    avg_node_strength      = avg_str,
    algebraic_connectivity = lam2
  )
}) %>% bind_rows()

# ──────────────────────────────────────────────────────────────────────────────
#  6. SHINY APP: UI & SERVER
# ──────────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("Interactive Catastrophic Fire Grid Explorer"),
  leafletOutput("map", height = "750px"),
  
  absolutePanel(
    top = 10, right = 10, width = 300,
    sliderInput("step", "Step",
                min = 1, max = steps, value = 1, step = 1,
                animate = animationOptions(1200)),
    plotOutput("resilience_plot", height = "300px"),
    verbatimTextOutput("resilience_metrics")
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(-125, 31, -102, 49)
  })
  
  output$resilience_plot <- renderPlot({
    ggplot(resilience_metrics_df, aes(x = step)) +
      geom_line(aes(y = load_served_pct,        color = "Load Served")) +
      geom_line(aes(y = gen_served_pct,         color = "Generation Served")) +
      geom_line(aes(y = 100 * (1 - buses_lost/nrow(bus_info_pre)),
                    color = "Buses Active")) +
      geom_line(aes(y = avg_node_strength,      color = "Avg Strength")) +
      geom_line(aes(y = algebraic_connectivity, color = "Algebraic Conn.")) +
      scale_color_manual(values = c(
        "Load Served"      = "blue",
        "Generation Served"= "green",
        "Buses Active"     = "orange",
        "Avg Strength"     = "purple",
        "Algebraic Conn."  = "brown"
      )) +
      labs(
        title = "Resilience Metrics Over Fire Progression",
        x     = "Simulation Step",
        y     = "% / Value",
        color = "Metric"
      ) +
      theme_minimal()
  })
  
  output$resilience_metrics <- renderPrint({
    row <- resilience_metrics_df[input$step, ]
    cat(
      "Step:", row$step, "\n",
      "----------------------------\n",
      "Islands:", row$islands, "\n",
      "Largest Island Size:", row$largest_island, "\n\n",
      "Network Resilience:\n",
      "----------------------------\n",
      "Total Buses Lost:", row$buses_lost, "of", nrow(bus_info_pre), "\n",
      "Load Served:", round(row$load_served_pct, 1), "%\n",
      "Generation Served:", round(row$gen_served_pct, 1), "%\n",
      "Avg Node Strength:", round(row$avg_node_strength, 2), "\n",
      "Algebraic Connectivity:", round(row$algebraic_connectivity, 3), "\n"
    )
  })
  
  observeEvent(input$step, {
    step   <- input$step
    g_cur  <- graph_sequence[[step]]
    active <- as.numeric(V(g_cur)$name)
    
    buses_step_sf <- buses_sf %>%
      filter(BusNum %in% active) %>%
      left_join(bus_info_pre, by = c("BusNum" = "bus_i")) %>%
      mutate(
        bus_color = bus_pal(bus_type),
        bus_label = paste0(
          "<strong>Bus ", BusNum, " - ", bus_name, "</strong><br/>",
          "Type: ", bus_type, "<br/>",
          "Load (Pd): ", Pd, " MW<br/>",
          "Generation: ", total_gen, " MW"
        )
      )
    
    lost_ids      <- buses_lost_per_step[[step]]
    lost_buses_sf <- buses_sf %>% filter(BusNum %in% lost_ids)
    edges_sf      <- prepare_edges_sf(g_cur, bus_data)
    
    gen_step_sf <- mpc_gen %>%
      filter(bus %in% active) %>%
      left_join(mpc_bus %>% select(bus_i, bus_name),
                by = c("bus" = "bus_i")) %>%
      left_join(bus_data, by = c("bus" = "BusNum")) %>%
      st_as_sf(coords = c("Longitude.1","Latitude.1"),
               crs = 4326, remove = FALSE) %>%
      mutate(
        gen_label = paste0(
          "<strong>Gen at Bus ", bus, " - ", bus_name, "</strong><br/>",
          "Type: ", fuel_type, "<br/>",
          "P<sub>g</sub>: ", Pg, " MW<br/>",
          "Max Cap: ", Pmax, " MW"
        )
      )
    
    proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolylines(data = edges_sf, color = "steelblue",
                   weight = 2, opacity = 0.7)
    
    if (length(burned_area_list[[step]]) > 0) {
      proxy <- proxy %>%
        addPolygons(data        = st_sf(geometry = burned_area_list[[step]]),
                    fillColor   = "orange", color = "red",
                    fillOpacity = 0.4, weight = 1,
                    popup       = paste0("Burned area, step ", step))
    }
    if (nrow(fire_points_list[[step]]) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(data        = fire_points_list[[step]],
                         radius      = 4, color = "darkred",
                         fillOpacity = 0.7,
                         label       = ~paste0("Burning Bus ", BusNum))
    }
    if (nrow(lost_buses_sf) > 2) {
      hull <- st_convex_hull(st_union(lost_buses_sf$geometry))
      proxy <- proxy %>%
        addPolygons(data        = st_sf(geometry = hull),
                    fillColor   = "gray", color = "black",
                    fillOpacity = 0.3, popup = "Power lost")
    }
    if (nrow(lost_buses_sf) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(data        = lost_buses_sf,
                         radius      = 5, color = "gray",
                         fillOpacity = 0.7,
                         label       = ~paste0("Bus ", BusNum, ": Power lost"))
    }
    
    proxy %>%
      addCircleMarkers(data        = buses_step_sf,
                       radius      = 5,
                       color       = ~bus_color,
                       stroke      = FALSE,
                       fillOpacity = 0.8,
                       label       = lapply(buses_step_sf$bus_label, HTML),
                       popup       = ~bus_label) %>%
      addCircleMarkers(data        = gen_step_sf,
                       radius      = 6,
                       fillColor   = ~fuel_pal(fuel_type),
                       color       = "#000000",
                       weight      = 0.5,
                       fillOpacity = 0.9,
                       popup       = ~gen_label) %>%
      addLegend(position = "topright", pal = fuel_pal,
                values   = mpc_gen$fuel_type,
                title    = "Generator Fuel Type", opacity = 0.8) %>%
      addLegend(position = "bottomright", pal = bus_pal,
                values   = buses_step_sf$bus_type,
                title    = "Bus Type", opacity = 0.8)
  })
}

shinyApp(ui, server)