# pp_ga_1.R
#
# Created: 2018-11-20
#  Author: Charles Zhu
#
# mTSP (path planning) solver
# GA solution, single chromosome
#
# References:
#   Mohammad Sedighpour, Majid Yousefikhoshbakht, and Narges Mahmoodi Darani.
#       "An effective genetic algorithm for solving
#           the multiple traveling salesman problem."
#       Journal of Optimization in Industrial Engineering 8 (2012): 73-79.
#       http://www.qjie.ir/article_89_0.html

if(!exists("EX_PP_GA_1_R")) {
    EX_PP_GA_1_R <<- TRUE

source("lib/basic.R")

suppressPackageStartupMessages(library(GA))

get_fitness_f_ga_1 <- function(
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost = NULL
    # this function needs to be called frequently, also note that
    # all parameters should have been checked (if necessary) in the main func
    # no paranoid check happens here
) {
    function(chromosome) {

    } # RETURN
}

ga_adjust_chromosome <- function(
    chromosome,
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost = NULL
) {

}

get_population_f_ga_1 <- function(
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost = NULL
) {
    function() {

    } # RETURN
}

get_crossover_f_ga_1 <- function(
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost = NULL
) {
    function() {

    } # RETURN
}

get_mutation_f_ga_1 <- function(
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost = NULL
) {
    function() {

    } # RETURN
}

get_multi_paths_ga_1 <<- function(
    l_selected,             # selected spots, len L vector
    distance_matrix,        # distance matrix
    max_cost_worker = +Inf, # max workload of any single worker
    spot_cali_cost = NULL,  # per-spot calibration cost
    paranoid = TRUE         # enable/disable paranoid checks
) {
    if(is.null(l_selected)) {
        l_selected <- rep(1L, NUM_SPOTS)
        l_selected[1] <- 0L
    }

    if(paranoid) {
        stopifnot(is.vector(l_selected))
        stopifnot(is.integer(l_selected))
        stopifnot(length(l_selected) == NUM_SPOTS)
        stopifnot(all(l_selected == 0L | l_selected == 1L))
        stopifnot(l_selected[1] == 0L)

        stopifnot(is.matrix(distance_matrix))
        stopifnot(is.integer(distance_matrix))
        stopifnot(ncol(distance_matrix) == NUM_SPOTS)
        stopifnot(nrow(distance_matrix) == NUM_SPOTS)
        stopifnot(all(distance_matrix) >= 0L)

        stopifnot(is.numeric(max_cost_worker))
        stopifnot(length(max_cost_worker) == 1L)
        stopifnot(max_cost_worker > 0)

        # if there is a workload constraint,
        # we will need to know the calibration cost at each spot,
        # which is equal to the maximum cali time among all selected sensors
        if(is.finite(max_cost_worker)) {
            stopifnot(is.vector(spot_cali_cost))
            stopifnot(is.numeric(spot_cali_cost))
            stopifnot(length(spot_cali_cost) == NUM_SPOTS)
            stopifnot(all(spot_cali_cost >= 0))
        }
    }
}

} # ENDIF
