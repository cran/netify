## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    dev = "png", dpi = 150,
    cache = FALSE,
    echo = TRUE,
    collapse = TRUE,
    comment = "#>"
)

## -----------------------------------------------------------------------------
library(netify)
data(icews)

## -----------------------------------------------------------------------------
net <- netify(icews[icews$year == 2010, ],
              actor1 = "i", actor2 = "j",
              symmetric = FALSE, weight = "verbCoop",
              nodal_vars = "i_polity2",
              dyad_vars  = "matlCoop")

class(net)
str(attributes(net), max.level = 1)

## -----------------------------------------------------------------------------
library(tibble)
library(broom)

# one row per dyad (long edge frame, wrapped in a tibble)
as_tibble(net)

# broom-style tidy summary: a tibble with one row per (non-zero) dyad
head(tidy(net))

# one-row-per-network model-card summary (one row per time/layer if applicable)
glance(net)

## -----------------------------------------------------------------------------
panel <- netify(icews[icews$year %in% c(2010, 2011), ],
                actor1 = "i", actor2 = "j", time = "year",
                symmetric = FALSE, weight = "verbCoop")
net_2010 <- subset_netify(panel, time = "2010")
net_2011 <- subset_netify(panel, time = "2011")
cmp <- compare_networks(list("2010" = net_2010, "2011" = net_2011))
as_tibble(cmp)

## -----------------------------------------------------------------------------
# class / structure predicates
is_netify(net)
is_bipartite(net)          # may be masked by igraph::is_bipartite
is_bipartite_netify(net)   # alias that won't be masked
is_directed_netify(net)
is_longitudinal(net)
is_multilayer(net)

# size / composition accessors
n_actors(net)              # number of unique actors
n_periods(net)             # number of time periods (1 for cross-sec)
n_layers(net)              # number of layers (1 for single-layer)
head(get_actor_time_info(net))   # stored actor_pds: actor, min_time, max_time

## -----------------------------------------------------------------------------
validate_netify(net, verbose = TRUE)

## -----------------------------------------------------------------------------
# clean netlet ticks every box
all(unlist(validate_netify(net, verbose = FALSE)))

# tamper: inject a stray actor into nodal_data
bad <- net
nd <- attr(bad, "nodal_data")
nd <- rbind(nd, nd[1, , drop = FALSE])
nd$actor[nrow(nd)] <- "ZZZ_not_in_network"
attr(bad, "nodal_data") <- nd

validate_netify(bad, verbose = TRUE)

## -----------------------------------------------------------------------------
set.seed(1)

# roster: actors with closed-interval entry / exit times
roster <- data.frame(
    actor = c("a", "b", "c", "d", "e"),
    min_time = c(1, 1, 1, 3, 3),
    max_time = c(2, 5, 4, 5, 5)   # a exits after t = 2
)

# edges (only show up while both endpoints are in the roster)
edges <- data.frame(
    i = c("a", "a", "b", "c", "d", "c", "d", "e"),
    j = c("b", "c", "c", "b", "e", "d", "e", "b"),
    t = c(1, 2, 2, 3, 4, 3, 5, 5)
)

net_oc <- netify(edges,
    actor1 = "i", actor2 = "j", time = "t",
    actor_time_uniform = FALSE,
    actor_pds = roster
)

# read the roster back off the netlet itself
head(get_actor_time_info(net_oc))
n_actors(net_oc)
n_periods(net_oc)

## -----------------------------------------------------------------------------
oc_summary <- summary(net_oc)
oc_summary[, c("net", "num_actors", "density", "num_edges")]

## -----------------------------------------------------------------------------
# example: number of weakly connected components with at least 2 nodes
n_components_2plus <- function(net) {
  g <- netify_to_igraph(net)
  c(n_components_2plus = sum(igraph::components(g)$csize >= 2))
}

# example: edge weight skewness
weight_skew <- function(net) {
  v <- as.vector(net)
  v <- v[!is.na(v) & v != 0]
  if (length(v) < 3) return(c(weight_skew = NA_real_))
  c(weight_skew = mean((v - mean(v))^3) / (stats::sd(v)^3))
}

summary(net, other_stats = list(
  comp = n_components_2plus,
  skew = weight_skew
))

## -----------------------------------------------------------------------------
# example: per-actor mean tie weight to non-isolates
mean_active_tie <- function(mat) {
  apply(mat, 1, function(row) {
    nonzero <- row[!is.na(row) & row != 0]
    if (length(nonzero) == 0) NA_real_ else mean(nonzero)
  })
}

head(summary_actor(net, stats = "fast", other_stats = list(mean_active = mean_active_tie)))

## -----------------------------------------------------------------------------
dd <- attr(net, "dyad_data")
names(dd)             # cross-sec: just "1"
names(dd[["1"]])      # one entry per dyadic variable
str(dd[["1"]][["matlCoop"]])

## ----eval = FALSE-------------------------------------------------------------
# # pseudo-structure for a 3-period network with 2 dyadic vars:
# # dd[["2010"]][["matlCoop"]]   -> n x n matrix
# # dd[["2010"]][["verbConf"]]   -> n x n matrix
# # dd[["2011"]][["matlCoop"]]   -> n x n matrix
# # ... etc.

## ----eval = FALSE-------------------------------------------------------------
# to_mymodel <- function(netlet, ...) {
#     validate_netify(netlet, verbose = FALSE)
#     layer_names <- attributes(netlet)$layers
#     if (length(layer_names) > 1) {
#         out <- lapply(layer_names, function(lyr) {
#             to_mymodel(subset_netify(netlet, layers = lyr), ...)
#         })
#         names(out) <- layer_names
#         return(out)
#     }
#     # ... single-layer logic ...
# }

## ----benchmark, eval = FALSE--------------------------------------------------
# library(netify)
# set.seed(1)
# bench_one <- function(N, p = 0.01) {
#   # build an er adjacency directly as an edgelist (skips the dense intermediate)
#   i <- sample.int(N, size = round(p * N * N), replace = TRUE)
#   j <- sample.int(N, size = length(i), replace = TRUE)
#   df <- data.frame(from = i, to = j)
# 
#   t0 <- Sys.time(); net <- netify(df, actor1 = "from", actor2 = "to"); t_build <- Sys.time() - t0
#   t0 <- Sys.time(); s   <- summary(net);                                 t_summary <- Sys.time() - t0
#   t0 <- Sys.time(); sa  <- summary_actor(net, stats = "fast");           t_actor_fast <- Sys.time() - t0
#   t0 <- Sys.time(); ig  <- to_igraph(net);                               t_igraph <- Sys.time() - t0
# 
#   data.frame(N = N,
#              build_s = as.numeric(t_build, units = "secs"),
#              summary_s = as.numeric(t_summary, units = "secs"),
#              summary_actor_fast_s = as.numeric(t_actor_fast, units = "secs"),
#              to_igraph_s = as.numeric(t_igraph, units = "secs"))
# }
# do.call(rbind, lapply(c(1000, 5000, 10000), bench_one))

