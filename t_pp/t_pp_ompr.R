#!/usr/bin/env Rscript
#
# t_pp_ompr.R
#
# Created: 2018-10-30
# Updated: 2018-11-04
#  Author: Charles Zhu
#
# test script for solution/pp_ompr.R
#
rm(list = ls())

library(magrittr)
suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(library(dplyr))

opt_list = list(
    make_option(
        c("--distance_file"),
        action = "store", default = NA, type = "character",
        help = "Distance (graph) filename"),
    make_option(
        c("--paranoid"),
        action = "store_true", default = FALSE, type = "logical",
        help = "Enable paranoid mode, default = %default"),
    make_option(
        c("--full_output"),
        action = "store_true", default = FALSE, type = "logical",
        help = "Enable human readable full output, default = %default"),
    make_option(
        c("--additional_field"),
        action = "store", default = NA, type = "character",
        help = "Additional field name(s), default = %default"),
    make_option(
        c("--additional_value"),
        action = "store", default = NA, type = "character",
        help = "Additional field value(s), default = %default")
)
opt_obj = OptionParser(option_list = opt_list)
opt = parse_args(opt_obj)
fo_flag = opt$full_output

if(is.na(opt$distance_file)) {
    print_help(object = opt_obj)
    stop("Must specify distance (graph) filename.")
}

stopifnot(
    is.na(opt$additional_field) == is.na(opt$additional_value))

if(fo_flag) {
    if(!is.na(opt$additional_field) || !is.na(opt$additional_value)) {
        stop("Additional field is not supported in full output.")
    }
    cat("Loading distance info...\n")
} else {
    cat(
        "path_planner",
        "num_spots",
        "solver_time",
        "solver_status",
        "objective_value",
        "num_workers",
        sep = ", "
    )
    if(!is.na(opt$additional_field)) {
        cat("", opt$additional_field, sep = ", ")
    }
    cat("\n")
}
load(opt$distance_file)
if(fo_flag) { cat(sprintf("Number of spots: %d\n", NUM_SPOTS))}

if(fo_flag) { cat("Loading path planner...\n") }
source("solution/pp_ompr.R")
if(fo_flag) {
    cat("Path planner: ompr\n")
} else {
    cat(
        "ompr",
        NUM_SPOTS,
        "",
        sep = ", "
    )
}

if(fo_flag) { cat("Solving...\n") }
solver_time = system.time(
    result <- get_multi_paths_ompr(
        l_selected = NULL,
        distance_matrix = map_graph_distances,
        num_workers = NULL,
        fix_workers = FALSE,
        max_cost_worker = 1200,
        spot_cali_cost = rep(0, NUM_SPOTS),
        paranoid = opt$paranoid
    )
)
res_status <- solver_status(result)
res_value <- objective_value(result)

if(fo_flag) {
    cat(sprintf("Time elapsed: %.3f\n", solver_time[3]))
    cat("Status:", res_status)
    cat("\nObjective value:", res_value)
    cat("\nDone!\n")
    cat("Solution:\n")
} else {
    cat(
        solver_time[3],
        res_status,
        res_value,
        "",
        sep = ", "
    )
}
solution <- get_solution(result, x[i, j, k]) %>% filter(value > 0 & i != j)
num_workers <- length(unique(solution$k))

if(fo_flag) {
    print(solution)
    cat(sprintf("Workers involved: %d\n", num_workers))
} else {
    cat(num_workers)
    if(!is.na(opt$additional_value)) {
        cat("", opt$additional_value, sep = ", ")
    }
    cat("\n")
}

