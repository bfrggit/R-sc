#!/usr/bin/env Rscript
#
# prep_graph_paris_full.R
#
# Created: 2019-01-03
#  Author: Charles Zhu
#
# as always, run this script from the project root

rm(list = ls())

source("lib/basic.R")

MOVEMENT_SPEED_PARIS <- 1   # m-per-sec
SPOT_ID_OFFSET <- 0L
lockBinding("MOVEMENT_SPEED_PARIS", globalenv())
lockBinding("SPOT_ID_OFFSET", globalenv())

# read pairwise travel distance data from raw scenario files
df_dist_paris <- read.table(
    file = "scenarios/raw_paris_tsv/route",
    header = FALSE,
    sep = ""
)
colnames(df_dist_paris) <- c(
    "spot_from",
    "spot_to",
    "street_distance",
    "unused_4",
    "unused_5"
)

# check and convert numerical spot ID into integer
stopifnot(all(df_dist_paris$spot_from == as.integer(df_dist_paris$spot_from)))
stopifnot(all(df_dist_paris$spot_to == as.integer(df_dist_paris$spot_to)))

storage.mode(df_dist_paris$spot_from) <- "integer"
storage.mode(df_dist_paris$spot_to) <- "integer"
df_dist_paris$spot_from <- df_dist_paris$spot_from - SPOT_ID_OFFSET + 1L
df_dist_paris$spot_to <- df_dist_paris$spot_to - SPOT_ID_OFFSET + 1L

# create map mat for each floor
NUM_SPOTS_FULL <- max(df_dist_paris[, 1L:2L])
map_paris <- matrix(0L, nrow = NUM_SPOTS_FULL, ncol = NUM_SPOTS_FULL)
for(rnd in 1L:nrow(df_dist_paris)) {
    map_paris[df_dist_paris$spot_from[rnd], df_dist_paris$spot_to[rnd]] <-
        map_paris[df_dist_paris$spot_to[rnd], df_dist_paris$spot_from[rnd]] <-
        as.integer(
            df_dist_paris$street_distance[rnd]
        )
}

save(
    df_dist_paris,
    map_paris,
    MOVEMENT_SPEED_PARIS,
    file = "scenarios/df_dist_map_paris.RData"
)

# fill in the adj matrix
map_graph_adj_matrix <- round(map_paris / MOVEMENT_SPEED_PARIS)
storage.mode(map_graph_adj_matrix) <- "integer"
colnames(map_graph_adj_matrix) <- rownames(map_graph_adj_matrix) <-
    z_nd_str("spot", NUM_SPOTS_FULL)

suppressPackageStartupMessages(library(igraph))

map_graph <- graph_from_adjacency_matrix(
    adjmatrix   = map_graph_adj_matrix,
    mode        = "directed",
    weighted    = TRUE,
    diag        = FALSE
)
map_graph_distances <- distances(
    map_graph,
    mode        = "out",
    algorithm   = "dijkstra"
)
storage.mode(map_graph_distances) <- "integer"
map_alternative_graph <- graph_from_adjacency_matrix(
    adjmatrix   = map_graph_distances,
    mode        = "directed",
    weighted    = TRUE,
    diag        = FALSE
)

save(
    NUM_SPOTS_FULL,
    map_graph_adj_matrix, map_graph,
    map_graph_distances, map_alternative_graph,
    file = "scenarios/graph_paris_full.RData"
)
