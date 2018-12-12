#!/usr/bin/env Rscript
#
# t_pp.R
#
# Created: 2018-12-03
# Updated: 2018-12-11
#  Author: Charles Zhu
#
# test script for mTSP solutions
#
rm(list = ls())

suppressPackageStartupMessages(require(optparse))

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
        c("--spot_cali_cost"),
        action = "store", default = 0L, type = "integer",
        help = "Calibration cost at any spot, default = %default"),
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
stopifnot(opt$spot_cali_cost >= 0)

PATH_PLANNERS <- c(
    "combined_1",
    "each",
    "ga_1",
    "ga_grd_1",
    "greedy_1",
    "ompr_glpk",
    "ompr_gurobi"
)
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
pp_f <- get(sprintf("get_multi_paths_%s", opt$path_planner))

if(fo_flag) {
    if(!is.na(opt$additional_field) || !is.na(opt$additional_value)) {
        stop("Additional field is not supported in full output.")
    }
    cat("Loading distance info...\n")
} else {
    cat(
        "path_planner",
        "num_spots",
        "max_cost_worker",
        "spot_cali_cost",
        "solver_time",
        "num_workers",
        "total_move_dist",
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
    cat("Maximum cost per worker:", opt$max_cost_worker, "\n")
    cat("Calibration cost at any spot:", opt$spot_cali_cost, "\n")
} else {
    cat(
        opt$path_planner,
        NUM_SPOTS,
        opt$max_cost_worker,
        opt$spot_cali_cost,
        "",
        sep = ", "
    )
}

l_selected <- c(0L, rep(1L, NUM_SPOTS - 1L))
spot_cali_cost <- c(0L, rep(opt$spot_cali_cost, NUM_SPOTS - 1L))

if(fo_flag) { cat("Solving...\n") }
solver_time = system.time(
    paths_array <- pp_f(
        l_selected      = l_selected,
        distance_matrix = map_graph_distances,
        max_cost_worker = opt$max_cost_worker,
        spot_cali_cost  = spot_cali_cost,
        paranoid        = opt$paranoid
    )
)

if(fo_flag) {
    cat(sprintf("Time elapsed: %.3f\n", solver_time[3]))
    cat("Done!\n")
    cat("Solution:\n")
} else {
    cat(
        solver_time[3],
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

tour_list <- NULL
if(is.null(paths_array)) {
    num_workers <- 0
    if(fo_flag) {
        cat("Could not find a solution under such constraints.\n")
    }
} else {
    num_workers <- dim(paths_array)[3]
    if(fo_flag) {
        # sparse_rep <- data.frame(
        #     which(paths_array > 0, arr.ind = TRUE),
        #     row.names = NULL
        # )
        # colnames(sparse_rep) <- c("from", "to", "worker")
        # sparse_rep <- sparse_rep[with(sparse_rep, order(worker, from)), ]
        # rownames(sparse_rep) <- NULL
        # print(sparse_rep)

        tour_list <- paths_array_to_tour_list(
            paths_array     = paths_array,
            start_from_spot = 1L,
            paranoid        = opt$paranoid
        )
        for(worker in 1L:num_workers) {
            cat(sprintf("%8s", sprintf("[%d]", worker)), "")
            cat(c(1L, tour_list[[worker]], 1L), sep = " -> ")
            cat("\n")
        }
    }
}

# use my implementation to compute cost per worker and total cost
# should be consistent with the objective value if no cali cost is introduced
move_dist_worker <- NULL
cali_cost_worker <- NULL
total_move_dist <- 0

if(!is.null(paths_array)) {
    if(opt$paranoid) {
        stopifnot(is.list(tour_list))
    }

    move_dist_worker <- get_move_dist_per_worker(
        distance_matrix = map_graph_distances,
        paths_array     = paths_array,
        paranoid        = opt$paranoid
    )
    cali_cost_worker <- unlist(
        lapply(
            tour_list,
            function(tour) {
                sum(spot_cali_cost[tour])
            }
        )
    )
    total_move_dist <- sum(move_dist_worker)
}

if(fo_flag) {
    cat(sprintf("Workers involved: %d\n", num_workers))
    cat("Move cost per worker:",
        paste(move_dist_worker, collapse = ", "), "\n")
    cat("Cali cost per worker:",
        paste(cali_cost_worker, collapse = ", "), "\n")
    cat("Total cost per worker:",
        paste(move_dist_worker + cali_cost_worker, collapse = ", "), "\n")
    cat("Total movement cost:", total_move_dist, "\n")
} else {
    cat(
        num_workers,
        total_move_dist,
        sep = ", "
    )

    if(!is.na(opt$additional_value)) {
        cat("", opt$additional_value, sep = ", ")
    }
    cat("\n")
}

