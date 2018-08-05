#!/usr/bin/env Rscript
#
# prep_location_unif.R
#
# Created: 2018-08-04
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
        c("-N", "--num_nodes"),
        action = "store", default = NA, type = "integer",
        help = "Number of nodes"),
    make_option(
        c("-L", "--num_spots"),
        action = "store", default = NA, type = "integer",
        help = "Number of spots")
)
opt_obj = OptionParser(option_list = opt_list)
opt = parse_args(opt_obj)

if(is.na(opt$output_file)) {
    print_help(object = opt_obj)
    stop("Must specify output filename.")
}
if(is.na(opt$num_nodes) || is.na(opt$num_spots)) {
    print_help(object = opt_obj)
    stop("Must specify number of nodes and number of spots.")
}

stopifnot(!file.exists(opt$output_file))
stopifnot(file.create(opt$output_file))
stopifnot(opt$num_nodes > 0L)
stopifnot(opt$num_spots > 1L)

NUM_NODES_LOCATED <<- opt$num_nodes
NUM_SPOTS_POPULATED <<- opt$num_spots
lockBinding("NUM_NODES_LOCATED", globalenv())
lockBinding("NUM_SPOTS_POPULATED", globalenv())

if(!is.na(opt$random_seed)) {
    set.seed(opt$random_seed)
}

source("lib/generator.R")

location_vector <- generate_sensor_location_vector_unif(
    num_nodes = NUM_NODES_LOCATED,
    num_spots = NUM_SPOTS_POPULATED
)
location_matrix <- conv_location_vector_to_matrix(
    location_vector = location_vector,
    num_spots       = NUM_SPOTS_POPULATED
)

save(
    NUM_NODES_LOCATED, NUM_SPOTS_POPULATED, location_vector, location_matrix,
    file = opt$output_file
)
