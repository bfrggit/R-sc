#!/usr/bin/env Rscript
#
# t_pp_ompr.R
#
# Created: 2018-10-30
# Updated: 2018-11-05
#  Author: Charles Zhu
#
# test script for solution/pp_ompr.R
#
rm(list = ls())

library(magrittr)
suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(library(dplyr))

source("lib/graph_aux.R")

opt_list = list(
    make_option(
        c("--distance_file"),
        action = "store", default = NA, type = "character",
        help = "Distance (graph) filename"),
    make_option(
        c("--max_cost_worker"),
        action = "store", default = +Inf, type = "numeric",
        help = "Maximum cost per worker, default = %default"),
    make_option(
        c("--path_planner"),
        action = "store", default = NA, type = "character",
        help = "Path planner name"
        ),
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

stopifnot(opt$max_cost_worker >= 0)

PATH_PLANNERS <- c("ompr_glpk")
lockBinding("PATH_PLANNERS", globalenv())

print_path_planners <- function() {
    cat("Supported path planner names: ")
    cat(PATH_PLANNERS, sep = ", ")
    cat("\n\n")
}

if(is.na(opt$path_planner)) {
    print_help(object = opt_obj)
    print_path_planners()
    stop("Must specify path planner name.")
}
if(!opt$path_planner %in% PATH_PLANNERS) {
    print_path_planners()
    stop(sprintf("Unsupported path planner name: %s", opt$path_planner))
}

stopifnot(
    is.na(opt$additional_field) == is.na(opt$additional_value))

# load specified path planner
if(fo_flag) { cat("Loading path planner...\n") }
source(sprintf("solution/pp_%s.R", opt$path_planner))

if(opt$path_planner == "ompr_glpk") {
    pp_f <- get_multi_paths_ompr_glpk
}
stopifnot(exists("pp_f"))

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
        "total_cost",
        sep = ", "
    )
    if(!is.na(opt$additional_field)) {
        cat("", opt$additional_field, sep = ", ")
    }
    cat("\n")
}
load(opt$distance_file)

if(fo_flag) {
    cat("Path planner:", opt$path_planner, "\n")
    cat(sprintf("Number of spots: %d\n", NUM_SPOTS))
} else {
    cat(
        opt$path_planner,
        NUM_SPOTS,
        "",
        sep = ", "
    )
}

if(fo_flag) { cat("Solving...\n") }
solver_time = system.time(
    paths_array <- pp_f(
        l_selected      = NULL,
        distance_matrix = map_graph_distances,
        num_workers     = NULL,
        fix_workers     = FALSE,
        max_cost_worker = opt$max_cost_worker,
        spot_cali_cost  = rep(0, NUM_SPOTS),
        paranoid        = opt$paranoid
    )
)
res_status <- solver_status(ompr_result)
res_value <- objective_value(ompr_result)

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

if(opt$paranoid) {
    stopifnot(
        is.null(paths_array) ||
        is_valid_multi_paths_array(
            paths_array             = paths_array,
            must_start_from_spot    = 1L,
            allow_zero              = FALSE,
            paranoid                = TRUE
        )
    )
}

if(is.null(paths_array)) {
    num_workers <- 0
    if(fo_flag) {
        cat("Could not find a solution under such constraints.\n")
    }
} else {
    num_workers <- dim(paths_array)[3]
    if(fo_flag) {
        sparse_rep <- data.frame(
            which(paths_array > 0, arr.ind = TRUE),
            row.names = NULL
        )
        colnames(sparse_rep) <- c("from", "to", "worker")
        sparse_rep <- sparse_rep[with(sparse_rep, order(worker, from)), ]
        rownames(sparse_rep) <- NULL
        print(sparse_rep)
    }
}

# use my implementation to compute cost per worker and total cost
# should be consistent with the objective value if no cali cost is introduced
cost_woker <- NULL
total_cost <- 0
if(!is.null(paths_array)) {
    cost_worker <- get_move_dist_per_worker(
        distance_matrix = map_graph_distances,
        paths_array     = paths_array,
        paranoid        = opt$paranoid
    )
    total_cost <- sum(cost_worker)

    # this is true only if no cali cost is introduced
    # manually check this in full output
    #
    # if(opt$paranoid) {
    #     stopifnot(total_cost == res_value)
    # }
}

if(fo_flag) {
    cat(sprintf("Workers involved: %d\n", num_workers))
    cat("Cost of each worker:", paste(cost_worker, collapse = ", "), "\n")
    cat("Total cost:", sum(cost_worker), "\n")
} else {
    cat(
        num_workers,
        total_cost,
        sep = ", "
    )

    if(!is.na(opt$additional_value)) {
        cat("", opt$additional_value, sep = ", ")
    }
    cat("\n")
}

