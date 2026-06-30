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

## -----------------------------------------------------------------------------
data(icews)
head(icews[, c("i", "j", "year", "verbCoop", "matlConf", "i_polity2", "i_region")])

## -----------------------------------------------------------------------------
verb_coop <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight   = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp", "i_region"),
    dyad_vars  = c("matlCoop", "verbConf")
)

print(verb_coop)

## -----------------------------------------------------------------------------
gs <- summary(verb_coop)
head(gs[, c("net", "num_actors", "density", "reciprocity", "mutual", "transitivity")])

## -----------------------------------------------------------------------------
as_ <- summary_actor(verb_coop)
head(as_[, c("actor", "time", "degree_in", "degree_out",
             "betweenness", "authority_score", "hub_score")])

## ----fig.width = 7, fig.height = 5--------------------------------------------
plot(verb_coop,
     time_filter = c("2004", "2008", "2012"),
     node_color_by = "i_region",
     edge_alpha = 0.1) +
  theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
hom <- homophily(verb_coop,
                 attribute = "i_polity2",
                 method = "correlation",
                 significance_test = FALSE)
head(hom)

## -----------------------------------------------------------------------------
mm <- mixing_matrix(verb_coop, attribute = "i_region", normalized = TRUE)
round(mm$mixing_matrices[[1]], 2)
mm$summary_stats[1, ]

## -----------------------------------------------------------------------------
temp_cmp <- compare_networks(verb_coop, method = "correlation")
head(temp_cmp$summary)

## -----------------------------------------------------------------------------
by_region <- compare_networks(
  subset(verb_coop, time = "2010"),
  by = "i_region",
  method = "correlation"
)
by_region$by_group$n_actors_per_group

## -----------------------------------------------------------------------------
ig <- to_igraph(verb_coop)             # list of igraph objects, one per year
ig_2010 <- ig[["2010"]]
length(igraph::cluster_walktrap(ig_2010))

## -----------------------------------------------------------------------------
df <- unnetify(subset(verb_coop, time = "2010"), remove_zeros = TRUE)
head(df[, c("from", "to", "verbCoop", "matlCoop", "i_polity2_from", "i_polity2_to")])

