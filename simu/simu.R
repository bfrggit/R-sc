#!/usr/bin/env Rscript
#
# simu.R
#
# Created: 2018-10-05
# Updated: 2018-12-09
#  Author: Charles Zhu
#
# derived from simu_no_move.R
#
rm(list = ls())

suppressPackageStartupMessages(require(optparse))

opt_list = list(
    make_option(
        c("-O", "--output_file"),
        action = "store", default = NA, type = "character",
        help = "Output filename, default = %default"),
    make_option(
        c("-W", "--num_iters"),
        action = "store", default = 100L, type = "integer",
        help = "Number of iterations, default = %default"),
    make_option(
        c("--sensor_file"),
        action = "store", default = NA, type = "character",
        help = "Sensor type definition filename"),
    make_option(
        c("--location_file"),
        action = "store", default = NA, type = "character",
        help = "Location filename"),
    make_option(
        c("--presence_file"),
        action = "store", default = NA, type = "character",
        help = "Presence filename"),
    make_option(
        c("--distance_file"),
        action = "store", default = NA, type = "character",
        help = "Distance (graph) filename"),
    make_option(
        c("--selector"),
        action = "store", default = NA, type = "character",
        help = "Selector name"),
    make_option(
        c("--path_planner"),
        action = "store", default = NA, type = "character",
        help = "Path planner name"),
    make_option(
        c("--max_cost_worker"),
        action = "store", default = +Inf, type = "numeric",
        help = "Maximum cost per worker, default = %default"),
    make_option(
        c("-x", "--weight_overhead"),
        action = "store", default = 1e+4, type = "numeric",
        help = "Weight of iteration overhead, default = %default"),
    make_option(
        c("-y", "--weight_cali"),
        action = "store", default = 1, type = "numeric",
        help = "Weight of calibration cost, default = %default"),
    make_option(
        c("-z", "--weight_move"),
        action = "store", default = 5, type = "numeric",
        help = "Weight of movement cost, default = %default"),
    make_option(
        c("-w", "--weight_worker"),
        action = "store", default = 0, type = "numeric",
        help = "Weight of worker overhead, default = %default"),
    make_option(
        c("--paranoid"),
        action = "store_true", default = FALSE, type = "logical",
        help = "Enable paranoid mode, default = %default"),
    make_option(
        c("--paranoid_path_planner"),
        action = "store_true", default = FALSE, type = "logical",
        help = "Enable paranoid mode for path planner, default = %default"),
    make_option(
        c("-v", "--verbose"),
        action = "store_true", default = FALSE, type = "logical",
        help = "Enable verbose output, default = %default"),
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

if(is.na(opt$sensor_file)) {
    print_help(object = opt_obj)
    stop("Must specify sensor type definition filename.")
}
if(is.na(opt$location_file)) {
    print_help(object = opt_obj)
    stop("Must specify location filename.")
}
if(is.na(opt$presence_file)) {
    print_help(object = opt_obj)
    stop("Must specify presence filename.")
}
if(is.na(opt$distance_file)) {
    print_help(object = opt_obj)
    stop("Must specify distance (graph) filename.")
}

SELECTORS <- c("minimal", "all", "nodal", "local")
lockBinding("SELECTORS", globalenv())

print_selectors <- function() {
    cat("Supported selector names: ")
    cat(SELECTORS, sep = ", ")
    cat("\n\n")
}

PATH_PLANNERS <- c("each", "ga_grd_1")
lockBinding("PATH_PLANNERS", globalenv())

print_path_planners <- function() {
    cat("Supported path planner names: ")
    cat(PATH_PLANNERS, sep = ", ")
    cat("\n\n")
}

if(is.na(opt$selector)) {
    print_help(object = opt_obj)
    print_selectors()
    stop("Must specify selector name.")
}
if(!opt$selector %in% SELECTORS) {
    print_selectors()
    stop(sprintf("Unsupported selector name: %s", opt$selector))
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

stopifnot(opt$max_cost_worker >= 0)

if(!is.na(opt$output_file)){
    stopifnot(!file.exists(opt$output_file))
    stopifnot(file.create(opt$output_file))
}
stopifnot(opt$num_iters > 0L)
stopifnot(opt$weight_overhead >= 0)
stopifnot(opt$weight_cali >= 0)
stopifnot(opt$weight_move >= 0)
stopifnot(opt$weight_worker >= 0)
stopifnot(
    is.na(opt$additional_field) == is.na(opt$additional_value))

num_iters <- opt$num_iters

source("lib/basic.R")
source("lib/naive_sel.R")
source("lib/run.R")

load(opt$sensor_file)
load(opt$location_file)
load(opt$presence_file)
load(opt$distance_file)

# prepare selectors and choose the one given by options
sel_f_all <- get_sel_f_all(s_presence = presence)
sel_f_nodal <- get_sel_f_nodal(s_presence = presence)
sel_f_local <- get_sel_f_local(
    n_location = location_matrix,s_presence = presence)
sel_f <- get(paste(c("sel_f", opt$selector), collapse = "_"))

# load specified path planner
source(sprintf("solution/pp_%s.R", opt$path_planner))
# if(opt$path_planner == "each") {
#     pp_f <- get_multi_paths_each
# }
# stopifnot(exists("pp_f"))
pp_f <- get(sprintf("get_multi_paths_%s", opt$path_planner))

# generate initial TTNC matrix
# in this test, all sensors are initially new
ttnc_init <- ifelse(
    presence,
    yes = matrix(
        rep(st_specs$st_period, NUM_NODES),
        nrow = NUM_NODES,
        byrow = TRUE
    ),
    no = Inf
)

cat(
    "selector",
    "num_iters",
    "max_cost_worker",
    "interval_mean",
    "cali_t_per_iter",
    "move_d_per_iter",
    "n_path_per_iter",
    "weight_overhead",
    "weight_cali",
    "weight_move",
    "weight_worker",
    "overhead_average",
    "cali_t_average",
    "move_d_average",
    "n_path_average",
    "weighted_overhead",
    "weighted_cali",
    "weighted_move",
    "weighted_worker",
    "weighted_sum",
    sep = ", "
)
if(!is.na(opt$additional_field)) {
    cat("", opt$additional_field, sep = ", ")
}
cat("\n")

#-----------------------------------------------------------
# actual function call to run simulator
#
res_case <- run(
    st_period   = st_specs$st_period,
    st_cali_t   = st_specs$st_cali_t,
    n_location  = location_matrix,
    s_presence  = presence,
    distance_matrix = map_graph_distances,
    ttnc_init   = ttnc_init,
    num_iters   = num_iters,
    selector_f  = sel_f,
    path_plan_f = pp_f,
    max_cost_worker = opt$max_cost_worker,
    paranoid    = opt$paranoid,
    pp_paranoid = opt$paranoid_path_planner,
    verbose     = opt$verbose
)

# compute simulation metrics
interval_mean <- mean(res_case$intervals)
cali_t_per_iter <- mean(res_case$cali_cost)
move_d_per_iter <- mean(res_case$move_cost)
n_path_per_iter <- mean(res_case$num_paths)
overhead_average <- num_iters / sum(res_case$intervals)
cali_t_average <- sum(res_case$cali_cost) / sum(res_case$intervals)
move_d_average <- sum(res_case$move_cost) / sum(res_case$intervals)
n_path_average <- sum(res_case$num_paths) / sum(res_case$intervals)
weighted_overhead <- opt$weight_overhead * overhead_average
weighted_cali <- opt$weight_cali * cali_t_average
weighted_move <- opt$weight_move * move_d_average
weighted_worker <- opt$weight_worker * n_path_average
weighted_sum <-
    weighted_overhead +
    weighted_cali +
    weighted_move +
    weighted_worker

# print results
cat(
    opt$selector,
    num_iters,
    opt$max_cost_worker,
    interval_mean,
    cali_t_per_iter,
    move_d_per_iter,
    n_path_per_iter,
    opt$weight_overhead,
    opt$weight_cali,
    opt$weight_move,
    opt$weight_worker,
    overhead_average,
    cali_t_average,
    move_d_average,
    n_path_average,
    weighted_overhead,
    weighted_cali,
    weighted_move,
    weighted_worker,
    weighted_sum,
    sep = ", "
)
if(!is.na(opt$additional_value)) {
    cat("", opt$additional_value, sep = ", ")
}
cat("\n")

# save to file
if(!is.na(opt$output_file)){
    save.image(file = opt$output_file)
}

# non-reachable section
# used for manual tests
if(FALSE) {

rm(list = ls())
load("prep_types_RData/types_10_1.RData")
load("prep_location_v_nodes_RData/location_100_1.RData")
load("prep_presence_v_prob_RData/presence_100_5_1.RData")
load("prep_graph_v_edges_RData/graph_300_4.RData")

source("lib/basic.R")
source("lib/naive_sel.R")
source("lib/run.R")
source("solution/pp_ga_grd_1.R")

num_iters <- 10L
pp_f <- get_multi_paths_ga_grd_1
sel_f <- sel_f_minimal
ttnc_init <- ifelse(
    presence,
    yes = matrix(
        rep(st_specs$st_period, NUM_NODES),
        nrow = NUM_NODES,
        byrow = TRUE
    ),
    no = Inf
)

res_case <- run(
    st_period   = st_specs$st_period,
    st_cali_t   = st_specs$st_cali_t,
    n_location  = location_matrix,
    s_presence  = presence,
    distance_matrix = map_graph_distances,
    ttnc_init   = ttnc_init,
    num_iters   = num_iters,
    selector_f  = sel_f,
    path_plan_f = pp_f,
    max_cost_worker = +Inf,
    paranoid    = TRUE,
    pp_paranoid = FALSE,
    verbose     = TRUE
)

}
