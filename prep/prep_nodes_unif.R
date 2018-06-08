#!/usr/bin/env Rscript
#
# prep_nodes_unif.R
#
# Created: 2018-06-07
#  Author: Charles Zhu

suppressPackageStartupMessages(require(optparse))

opt_list = list(
    make_option(
        c("-n", "--num_cases"),
        action = "store", default = 10, type = "integer",
        help = "Number of test cases to generate, default = %default"),
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

if(is.na(opt$num_nodes) || is.na(opt$num_types)) {
    print_help(object = opt_obj)
    stop("Must specify number of nodes and number of types.")
}

stopifnot(opt$num_cases > 0L)
stopifnot(opt$num_nodes > 0L)
stopifnot(opt$num_types > 0L)
stopifnot(opt$prob > 0L && opt$prob <= 100L)

NUM_NODES <<- opt$num_nodes
NUM_TYPES_PER_NODE <<- opt$num_types
lockBinding("NUM_NODES", globalenv())
lockBinding("NUM_TYPES_PER_NODE", globalenv())

seed <-4
set.seed(seed)
dir.create(path = "prep_RData", showWarnings = FALSE)

source("lib/generator.R")

for(jnd in 1L:opt$num_cases) {
    presence <- generate_sensor_presence(
        num_nodes = NUM_NODES,
        num_types = NUM_TYPES_PER_NODE,
        prob      = opt$prob / 100.0
    )

    save(
        NUM_NODES, NUM_TYPES_PER_NODE, presence,
        file = sprintf(
            "prep_RData/p_%03x%03x%02x_unif_%02d.RData",
            NUM_TYPES_PER_NODE, NUM_NODES,
            opt$prob,
            jnd
        )
    )
}
