#!/usr/bin/env Rscript
#
# prep_graph_random_loop_unif.R
#
# Created: 2018-09-24
# Updated: 2018-10-03
#  Author: Charles Zhu

suppressPackageStartupMessages(require(optparse))

opt_list = list(
    make_option(
        c("-O", "--output_file"),
        action = "store", default = NA, type = "character",
        help = "Output filename"),
    make_option(
        c("-s", "--random_seed"),
        action = "store", default = NA, type = "integer",
        help = "Random seed, default = %default"),
    make_option(
        c("-L", "--num_spots"),
        action = "store", default = NA, type = "integer",
        help = "Number of spots"),
    make_option(
        c("-e", "--num_edges"),
        action = "store", default = NA, type = "integer",
        help = "Number of edges"),
    make_option(
        c("--weight_lower"),
        action = "store", default = 30, type = "integer",
        help = "Minimum edge weight (movement time), default = %default"),
    make_option(
        c("--weight_upper"),
        action = "store", default = 300, type = "integer",
        help = "Maximum edge weight (movement time), default = %default")
)
opt_obj = OptionParser(option_list = opt_list)
opt = parse_args(opt_obj)

if(is.na(opt$output_file)) {
    print_help(object = opt_obj)
    stop("Must specify output filename.")
}
if(is.na(opt$num_spots) || is.na(opt$num_edges)) {
    print_help(object = opt_obj)
    stop("Must specify number of spots and number of edges.")
}

stopifnot(!file.exists(opt$output_file))
stopifnot(file.create(opt$output_file))
stopifnot(opt$num_spots > 1L)
stopifnot(opt$num_edges >= opt$num_spots)
stopifnot(opt$num_edges <= opt$num_spots * opt$num_spots - opt$num_spots)

NUM_SPOTS <<- opt$num_spots
NUM_EDGES <<- opt$num_edges
lockBinding("NUM_SPOTS", globalenv())
lockBinding("NUM_EDGES", globalenv())

if(!is.na(opt$random_seed)) {
    set.seed(opt$random_seed)
}

suppressPackageStartupMessages(source("lib/graph_generator.R"))

WEIGHT_GEN_F <<- function(size = 1L) {
    as.integer(
        sample(
            opt$weight_lower:opt$weight_upper,
            size = size,
            replace = TRUE
        )
    ) # RETURN
}
lockBinding("WEIGHT_GEN_F", globalenv())

map_graph_adj_matrix <- generate_connected_digraph_adj_matrix_random_loop(
    num_spots   = NUM_SPOTS,
    num_edges   = opt$num_edges,
    rand_gen_f  = WEIGHT_GEN_F
)
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
    NUM_SPOTS, NUM_EDGES, WEIGHT_GEN_F,
    map_graph_adj_matrix, map_graph,
    map_graph_distances, map_alternative_graph,
    file = opt$output_file
)
