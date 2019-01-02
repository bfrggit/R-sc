#!/usr/bin/env Rscript
#
# prep_graph_cs3_full.R
#
# Created: 2018-12-31
# Updated: 2019-01-01
#  Author: Charles Zhu
#
# as always, run this script from the project root

rm(list = ls())

source("lib/basic.R")

MAP_SCALE_FACTOR_CS3 <- 32L # ft-per-unit
MOVEMENT_SPEED_CS3 <- 3     # ft-per-sec
FLOOR_PLANS_CS3 <- c(1L, 2L, 3L, 2L, 3L, 4L)
NUM_FLOORS_CS3 <- length(FLOOR_PLANS_CS3)
MOVEMENT_COST_UPSTAIR <- 45L
MOVEMENT_COST_DOWNSTAIR <- 30L

lockBinding("MAP_SCALE_FACTOR_CS3", globalenv())
lockBinding("MOVEMENT_SPEED_CS3", globalenv())
lockBinding("FLOOR_PLANS_CS3", globalenv())
lockBinding("NUM_FLOORS_CS3", globalenv())
lockBinding("MOVEMENT_COST_UPSTAIR", globalenv())
lockBinding("MOVEMENT_COST_DOWNSTAIR", globalenv())

ls_df_dist_cs3 <- list()

# read pairwise travel distance data from raw scenario files
ls_df_dist_cs3$flr01 <-
    read.csv(file = "scenarios/raw_cs3_csv/distances_cs3_flr01.csv")
ls_df_dist_cs3$flr02 <-
    read.csv(file = "scenarios/raw_cs3_csv/distances_cs3_flr02.csv")
ls_df_dist_cs3$flr03 <-
    read.csv(file = "scenarios/raw_cs3_csv/distances_cs3_flr03.csv")
ls_df_dist_cs3$flr06 <-
    read.csv(file = "scenarios/raw_cs3_csv/distances_cs3_flr06.csv")

save(
    ls_df_dist_cs3,
    MAP_SCALE_FACTOR_CS3,
    file = "scenarios/ls_df_dist_cs3.RData"
)

# create map mat for each floor
ls_map_cs3 <- list()
for(jnd in 1L:length(ls_df_dist_cs3)) {
    df_dist_flr <- ls_df_dist_cs3[[jnd]]
    num_spots_flr <- max(df_dist_flr[, 1L:2L])
    map_flr <- matrix(0L, nrow = num_spots_flr, ncol = num_spots_flr)
    for(rnd in 1L:nrow(df_dist_flr)) {
        map_flr[df_dist_flr$spot_from[rnd], df_dist_flr$spot_to[rnd]] <-
            map_flr[df_dist_flr$spot_to[rnd], df_dist_flr$spot_from[rnd]] <-
            as.integer(
                df_dist_flr$map_scale_distance[rnd] * MAP_SCALE_FACTOR_CS3
            )
    }
    ls_map_cs3[[jnd]] <- map_flr
}
names(ls_map_cs3) <- names(ls_df_dist_cs3)

save(
    ls_map_cs3,
    MOVEMENT_SPEED_CS3,
    FLOOR_PLANS_CS3,
    NUM_FLOORS_CS3,
    MOVEMENT_COST_UPSTAIR,
    MOVEMENT_COST_DOWNSTAIR,
    file = "scenarios/ls_map_cs3.RData"
)

# manually generate graph mat
num_spots_flr <- unlist(lapply(ls_map_cs3, FUN = nrow))[FLOOR_PLANS_CS3]
names(num_spots_flr)[c(4L, 5L)] <- c("flr04", "flr05") # hard-coded name change
spot_id_upper <- cumsum(num_spots_flr)
spot_id_lower <- c(1L, spot_id_upper[-length(spot_id_upper)] + 1L)
names(spot_id_lower) <- names(spot_id_upper)
NUM_SPOTS_FULL <- spot_id_upper[length(spot_id_upper)]

stopifnot(length(num_spots_flr) == NUM_FLOORS_CS3)
stopifnot(length(spot_id_lower) == NUM_FLOORS_CS3)
stopifnot(length(spot_id_upper) == NUM_FLOORS_CS3)

# fill in the adj matrix
map_graph_adj_matrix <- matrix(0L, nrow = NUM_SPOTS_FULL, ncol = NUM_SPOTS_FULL)
for(jnd in 1L:NUM_FLOORS_CS3) {
    spot_id_affected <- spot_id_lower[jnd]:spot_id_upper[jnd]
    map_graph_adj_matrix[spot_id_affected, spot_id_affected] <-
        as.integer(
            round(
                ls_map_cs3[[FLOOR_PLANS_CS3[jnd]]] / MOVEMENT_SPEED_CS3
            )
        )
}
colnames(map_graph_adj_matrix) <- rownames(map_graph_adj_matrix) <-
    z_nd_str("spot", NUM_SPOTS_FULL)

# manually add staircase movement
staircase_id_cs3 <- list()
staircase_id_cs3[[1L]] <- c(3, 1, 1, 1)
staircase_id_cs3[[2L]] <- c(4, 2, 2, 2)
staircase_id_cs3[[3L]] <- c(2, 3, 3, NA)
for(staircase_nd in 1L:3L) {
    staircase_id <- staircase_id_cs3[[staircase_nd]]
    for(jnd in 1L:NUM_FLOORS_CS3) {
        spot_from <-
            staircase_id[FLOOR_PLANS_CS3[jnd]] +
            spot_id_lower[jnd] - 1L
        spot_to <-
            staircase_id[FLOOR_PLANS_CS3[jnd + 1L]] +
            spot_id_lower[jnd + 1L] - 1L
        if(is.finite(spot_from) && is.finite(spot_to)) {
            map_graph_adj_matrix[spot_from, spot_to] <- MOVEMENT_COST_UPSTAIR
            map_graph_adj_matrix[spot_to, spot_from] <- MOVEMENT_COST_DOWNSTAIR
        }
    }
}

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
    file = "scenarios/graph_cs3_full.RData"
)
