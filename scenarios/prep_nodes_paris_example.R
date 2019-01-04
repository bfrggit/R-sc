#!/usr/bin/env Rscript
#
# prep_nodes_paris_example.R
#
# Created: 2019-01-03
#  Author: Charles Zhu
#
# as always, run this script from the project root

rm(list = ls())

source("lib/basic.R")

SPOT_ID_OFFSET <- 2L
NODE_ID_OFFSET <- 1L
lockBinding("SPOT_ID_OFFSET", globalenv())
lockBinding("NODE_ID_OFFSET", globalenv())

# find out total number of spots and types
load("scenarios/df_dist_map_paris.RData")
load("scenarios/types_paris.RData")
NUM_SPOTS_POPULATED <- max(df_dist_paris[, 1L:2L])
NUM_TYPES_PER_NODE <- NUM_TYPES
lockBinding("NUM_SPOTS_POPULATED", globalenv())
lockBinding("NUM_TYPES_PER_NODE", globalenv())

stopifnot(nrow(map_paris) == NUM_SPOTS_POPULATED)
stopifnot(ncol(map_paris) == NUM_SPOTS_POPULATED)

df_nodes_paris <- read.table(
    file = "scenarios/raw_paris_tsv/nodes",
    header = FALSE,
    sep = "",
    fill = TRUE,
    col.names = c(
        "node_id", "spot_id", "spot_lat", "spot_lon", "spot_alt",
        z_nd_str("var", 10L)
    )
)

# check and convert numerical columns into integer
stopifnot(all(df_nodes_paris$spot_id == as.integer(df_nodes_paris$spot_id)))
stopifnot(all(df_nodes_paris$node_id == as.integer(df_nodes_paris$node_id)))

storage.mode(df_nodes_paris$spot_id) <- "integer"
storage.mode(df_nodes_paris$node_id) <- "integer"
for(cnd in 6L:15L) {
    storage.mode(df_nodes_paris[, cnd]) <- "integer"
}
df_nodes_paris$spot_id <- df_nodes_paris$spot_id + SPOT_ID_OFFSET
df_nodes_paris$node_id <- df_nodes_paris$node_id + NODE_ID_OFFSET

# check if every node has a spot assignment
df_nodes_paris <- df_nodes_paris[order(df_nodes_paris$node_id), ]
NUM_NODES <- NUM_NODES_LOCATED <- max(df_nodes_paris$node_id)
lockBinding("NUM_NODES_LOCATED", globalenv())
lockBinding("NUM_NODES", globalenv())

stopifnot(df_nodes_paris$node_id[1L] == 1L)
stopifnot(nrow(df_nodes_paris) == NUM_NODES_LOCATED)

# generate location info in my format
location_vector <- df_nodes_paris$spot_id
names(location_vector) <- z_nd_str("node", NUM_NODES_LOCATED)

source("lib/generator.R")

location_matrix <- conv_location_vector_to_matrix(
    location_vector = location_vector,
    num_spots       = NUM_SPOTS_POPULATED,
    paranoid        = TRUE
)

save(
    NUM_NODES_LOCATED, NUM_SPOTS_POPULATED, location_vector, location_matrix,
    file = "scenarios/location_paris_example.RData"
)

presence <- matrix(0L, nrow = NUM_NODES, ncol = NUM_TYPES_PER_NODE)
for(rnd in 1L:nrow(df_nodes_paris)) {
    avail_sensors <- unlist(df_nodes_paris[rnd, -(1L:5L)])
    avail_sensors <- avail_sensors[!is.na(avail_sensors)]
    presence[df_nodes_paris$node_id[rnd], avail_sensors] = 1L
}
colnames(presence) = z_nd_str("type", NUM_TYPES_PER_NODE)
rownames(presence) = z_nd_str("node", NUM_NODES)

save(
    NUM_NODES, NUM_TYPES_PER_NODE, presence,
    file = "scenarios/presence_paris_example.RData"
)
