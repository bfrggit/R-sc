#!/usr/bin/env Rscript
#
# prep_presence_unif.R
#
# Created: 2018-08-01
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
        c("-K", "--num_types"),
        action = "store", default = NA, type = "integer",
        help = "Number of types"),
    make_option(
        c("-N", "--num_nodes"),
        action = "store", default = NA, type = "integer",
        help = "Number of nodes"),
    make_option(
        c("-p", "--prob"),
        action = "store", default = 50, type = "integer",
        help = paste(c(
            "Percentage probability of presence for a node-type pair",
            ", default = %default"), collapse = ""))
)
opt_obj = OptionParser(option_list = opt_list)
opt = parse_args(opt_obj)

if(is.na(opt$output_file)) {
    print_help(object = opt_obj)
    stop("Must specify output filename.")
}
if(is.na(opt$num_nodes) || is.na(opt$num_types)) {
    print_help(object = opt_obj)
    stop("Must specify number of nodes and number of types.")
}

stopifnot(!file.exists(opt$output_file))
stopifnot(file.create(opt$output_file))
stopifnot(opt$num_nodes > 0L)
stopifnot(opt$num_types > 0L)
stopifnot(opt$prob > 0L && opt$prob <= 100L)

NUM_NODES <<- opt$num_nodes
NUM_TYPES_PER_NODE <<- opt$num_types
lockBinding("NUM_NODES", globalenv())
lockBinding("NUM_TYPES_PER_NODE", globalenv())

if(!is.na(opt$random_seed)) {
    set.seed(opt$random_seed)
}

source("lib/generator.R")

presence <- generate_sensor_presence_unif(
    num_nodes = NUM_NODES,
    num_types = NUM_TYPES_PER_NODE,
    prob      = opt$prob / 100.0
)

save(
    NUM_NODES, NUM_TYPES_PER_NODE, presence,
    file = opt$output_file
)
