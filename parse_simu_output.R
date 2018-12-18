#!/usr/bin/env Rscript
#
# parse_simu_output.R
#
# Created: 2018-12-17
# Author: Charles Zhu
#
rm(list = ls())

suppressPackageStartupMessages(require(optparse))

parse_opt_list = list(
    make_option(
        c("-I", "--input_file"),
        action = "store", default = NA, type = "character",
        help = "Input (simulation output) filename, default = %default"),
    make_option(
        c("--output_directory"),
        action = "store", default = NA, type = "character",
        help = "Output dirname, default = %default")
)
parse_opt_obj = OptionParser(option_list = parse_opt_list)
parse_opt = parse_args(parse_opt_obj)

if(is.na(parse_opt$input_file)) {
    print_help(object = parse_opt_obj)
    stop("Must specify input (simulation output) filename.")
}
if(is.na(parse_opt$output_directory)) {
    print_help(object = parse_opt_obj)
    stop("Must specify output dirname.")
}

cat(sprintf("Loading simulation output file: %s\n", parse_opt$input_file))
load(parse_opt$input_file)
if(exists("EX_GRAPH_AUX_R")) { rm("EX_GRAPH_AUX_R") }

source("lib/graph_aux.R")

out_dir <- parse_opt$output_directory
cat(sprintf("Creating directory: %s\n", out_dir))
dir.create(path = out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(dir.exists(out_dir))

stopifnot(length(res_case$history_selection) == res_case$num_iters)
stopifnot(length(res_case$history_paths) == res_case$num_iters)
cat(sprintf("Found %d iterations\n", res_case$num_iters))
for(it in 1L:res_case$num_iters) {
    cat(sprintf("Processing interation: %d\n", it))
    tour_list <- paths_array_to_tour_list(
        paths_array     = res_case$history_paths[[it]],
        start_from_spot = 1L,
        paranoid        = TRUE
    )
    selection <- res_case$history_selection[[it]]

    stopifnot(length(tour_list) > 0)

    cat("---\n")
    for(tour_jnd in 1L:length(tour_list)) {
        t_tour <- tour_list[[tour_jnd]]
        cat(
            sprintf("WORKER %d", tour_jnd),
            "HAS SPOTS IN ORDER",
            sprintf("%s\n", paste(t_tour, collapse = " "))
        )
        for(t_spot in t_tour) {
            t_spot_nodes <- which(location_matrix[, t_spot] > 0)
            cat(
                sprintf("SPOT %d", t_spot),
                "HAS NODES",
                sprintf("%s\n", paste(t_spot_nodes, collapse = " "))
            )
            for(t_node in t_spot_nodes) {
                t_node_sensors <- which(selection[t_node, ] > 0)
                cat(
                    sprintf("NODE %d", t_node),
                    "HAS SENSORS",
                    sprintf("%s\n", paste(t_node_sensors, collapse = " "))
                )
            }
        }
    }
    cat("---\n")
}
cat("Done!", "\n")
