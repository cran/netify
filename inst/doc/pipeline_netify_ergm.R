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
library(ggplot2)
library(network)
library(ergm)

data(icews)

## -----------------------------------------------------------------------------
# build a single-year netify object with nodal and dyadic attributes
icews_2010 <- icews[icews$year == 2010, ]

verb_coop <- netify(
    icews_2010,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight   = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp"),
    dyad_vars  = "matlCoop"
)

# convert to statnet 'network' object
sn_2010 <- to_statnet(verb_coop)
sn_2010

## -----------------------------------------------------------------------------
# 1. nas in nodal covariates referenced by nodecov / nodematch
nd <- attr(verb_coop, "nodal_data")
na_cols <- names(nd)[vapply(nd, function(c) any(is.na(c)), logical(1))]
na_cols <- setdiff(na_cols, c("actor", "time", "layer"))
na_cols

# 2. isolates -- ergm fits but some terms (e.g. gwdegree) degenerate
m <- get_raw(verb_coop)
bin <- (m != 0) & !is.na(m)
deg_total <- rowSums(bin) + colSums(bin)
isolates <- rownames(m)[deg_total == 0]
length(isolates)

# 3. symmetry: the netify flag must match how your model treats ties
attr(verb_coop, "symmetric")

## -----------------------------------------------------------------------------
clean_cols <- attr(sn_2010, "netify_na_cols")
if (is.null(clean_cols)) clean_cols <- na_cols
if (!is.null(clean_cols) && length(clean_cols) > 0) {
    verb_coop_clean <- drop_na_actors(verb_coop, cols = clean_cols)
} else {
    verb_coop_clean <- verb_coop
}

sn_2010_clean <- to_statnet(verb_coop_clean)
attr(sn_2010_clean, "netify_na_cols")

## ----eval = FALSE-------------------------------------------------------------
# # note: this chunk is not evaluated by default to keep vignette build fast.
# # replace eval = false with eval = true to actually run.
# set.seed(6886)
# m <- ergm(
#     sn_2010_clean ~ edges +
#         nodecov("i_polity2") +
#         nodecov("i_log_gdp")
# )
# summary(m)

## -----------------------------------------------------------------------------
toy_edges <- data.frame(
    i = c("a", "a", "b", "c"),
    j = c("b", "c", "c", "d"),
    y = 1
)
toy_net <- suppressMessages(netify(
    toy_edges,
    actor1 = "i", actor2 = "j",
    weight = "y", symmetric = FALSE
))
toy_sn <- to_statnet(toy_net)
set.seed(6886)
invisible(capture.output(
    toy_fit <- ergm(toy_sn ~ edges, estimate = "MPLE", eval.loglik = FALSE)
))
coef(toy_fit)

## ----eval = FALSE-------------------------------------------------------------
# m <- ergm(sn_2010 ~ edges + edgecov("matlCoop"))

## -----------------------------------------------------------------------------
verb_longit <- netify(
    icews[icews$year %in% 2010:2012, ],
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = "i_polity2"
)

sn_list <- to_statnet(verb_longit)
length(sn_list)
names(sn_list)
class(sn_list[[1]])

## ----eval = FALSE-------------------------------------------------------------
# set.seed(6886)
# fits <- lapply(sn_list, function(n) {
#     ergm(n ~ edges)
# })

## ----eval = FALSE-------------------------------------------------------------
# # longitudinal ergm via tergm 4.x
# library(tergm)
# set.seed(6886)
# 
# nets <- to_statnet(verb_longit)   # named list of network objects
# fit <- tergm(
#     nets ~ Form(~ edges) +
#            Persist(~ edges),
#     estimate = "CMLE",
#     times = seq_along(nets)
# )
# summary(fit)

## -----------------------------------------------------------------------------
verb <- netify(icews_2010, actor1 = "i", actor2 = "j",
               symmetric = FALSE, weight = "verbCoop")
matl <- netify(icews_2010, actor1 = "i", actor2 = "j",
               symmetric = FALSE, weight = "matlCoop")
multi <- layer_netify(list(verbal = verb, material = matl))

sn_multi <- to_statnet(multi)
length(sn_multi)
names(sn_multi)

## ----eval = FALSE-------------------------------------------------------------
# set.seed(6886)
# # m is the fitted ergm object from the model chunk above
# sims <- simulate(m, nsim = 100)  # list of network objects
# 
# # convert each simulated network back into a netify for comparison
# sim_nets <- lapply(sims, function(s) to_netify(s))
# 
# # compare observed vs simulated at the structural level
# all_nets <- c(list(observed = verb_coop), sim_nets)
# struct_comp <- compare_networks(all_nets, what = "structure")

