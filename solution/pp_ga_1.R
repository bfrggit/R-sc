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

# get_tour_len <- function(
#     tour_vec,           # vector of spots to visit in order except for depot
#     distance_matrix,
#     start_from_spot = 1L
# ) {
#     num_tour <- length(tour_vec) # number of non-depot spots in this tour
#     tour_sum <- 0
#
#     # an empty tour has length 0
#     if(num_tour == 0) { return(tour_sum) }
#
#     # going from and back to depot is enough, if there is only one spot
#     tour_sum <- tour_sum + distance_matrix[start_from_spot, tour_vec[1L]]
#     tour_sum <- tour_sum + distance_matrix[tour_vec[num_tour], start_from_spot]
#     if(num_tour == 1) { return(tour_sum) }
#
#     # add distance between consecutive pairs of spots in the given tour
#     for(j in 1L:(num_tour - 1L)) {
#         tour_sum <- tour_sum + distance_matrix[tour_vec[j], tour_vec[j + 1L]]
#     }
#     tour_sum # RETURN
# }
#
get_tour_len <- function(
    chromosome,
    tour_begin,
    tour_end,
    distance_matrix,
    start_from_spot = 1L
) {
    num_tour <- tour_end - tour_begin + 1L # number of non-depot spots
    tour_sum <- 0

    # an empty tour has length 0
    if(num_tour < 1) { return(tour_sum) }

    # going from and back to depot is enough, if there is only one spot
    tour_sum <- tour_sum + distance_matrix[
        start_from_spot,
        chromosome[tour_begin]
    ]
    tour_sum <- tour_sum + distance_matrix[
        chromosome[tour_end],
        start_from_spot
    ]
    if(num_tour == 1) { return(tour_sum) }

    # add distance between consecutive pairs of spots in the given tour
    for(j in tour_begin:(tour_end - 1L)) {
        tour_sum <- tour_sum + distance_matrix[
            chromosome[j],
            chromosome[j + 1L]
        ]
    }
    tour_sum # RETURN
}

get_fitness_f_ga_1 <- function(
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost  = NULL,
    start_from_spot = 1L,
    paranoid        = TRUE
) {
    if(paranoid) {
        # most paranoid checks should have been done in main func
        stopifnot(is.infinite(max_cost_worker) || is.vector(spot_cali_cost))
    }
    num_selected <- as.integer(sum(l_selected > 0))

    # this function needs to be called frequently, also note that
    # all parameters should have been checked (if necessary) in the main func
    # very little paranoid check happens here
    function(chromosome) {
        # length of chromosome should be (2 * num_selected)
        # its left half contains tours represented in spot numbers
        # its right half contains tour length of salesmen
        if(paranoid) {
            stopifnot(length(chromosome) == 2 * num_selected)
            stopifnot(
                sum(
                    chromosome[
                        (num_selected + 1L):
                        (2L * num_selected)
                    ]
                ) == num_selected
            )
        }

        # compute begin/end indexes of each tour in the chromosome
        tour_end <- as.integer(
            cumsum(
                chromosome[
                    (num_selected + 1L):
                    (2L * num_selected)
                ]
            )
        )
        tour_begin <- 1L
        if(num_selected > 1) {
            tour_begin <- c(
                tour_begin,
                tour_end[1L:(num_selected - 1L)] + 1L
            )
        }

        multi_sum <- 0
        for(tnd in 1L:num_selected) {
            if(chromosome[num_selected + tnd] > 0) {
                multi_sum <- multi_sum + get_tour_len(
                    chromosome      = chromosome,
                    tour_begin      = tour_begin[tnd],
                    tour_end        = tour_end[tnd],
                    distance_matrix = distance_matrix,
                    start_from_spot = start_from_spot
                )
            }
        }
        multi_sum # RETURN
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
