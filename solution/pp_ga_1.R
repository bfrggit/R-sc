# pp_ga_1.R
#
# Created: 2018-11-20
# Updated: 2018-11-26
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

is_valid_chromosome <- function(chromosome, start_from_spot) {
    stopifnot(is.integer(start_from_spot))
    stopifnot(length(start_from_spot) == 1L)
    stopifnot(start_from_spot > 0)

    if(!is.vector(chromosome)) return(FALSE)
    if(!is.numeric(chromosome)) return(FALSE)
    if(any(as.integer(chromosome) != chromosome)) return(FALSE)
    if(length(chromosome) %% 2L != 0) return(FALSE)

    num_selected <- length(chromosome) %/% 2L
    order_spots <- chromosome[1L:num_selected]
    if(any(order_spots < 1)) return(FALSE)
    if(any(order_spots > NUM_SPOTS)) return(FALSE)
    if(any(order_spots == start_from_spot)) return(FALSE)
    if(length(unique(order_spots)) != length(order_spots)) return(FALSE)

    num_tour <- chromosome[(num_selected + 1L):length(chromosome)]
    if(length(num_tour) != num_selected) return(FALSE)
    if(any(num_tour < 0)) return(FALSE)
    if(sum(num_tour) != num_selected) return(FALSE)

    TRUE # RETURN
}

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
    start_from_spot,
    paranoid = TRUE
) {
    if(paranoid) {
        stopifnot(is_valid_chromosome(chromosome, start_from_spot))
        num_selected <- length(chromosome) %/% 2L

        stopifnot(tour_begin <= num_selected)
        stopifnot(tour_end <= num_selected)
    }

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

# compute begin/end indexes of each tour in the chromosome
get_tour_begin_end <- function(
    chromosome,
    num_selected
) {
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

    list(
        tour_begin  = tour_begin,
        tour_end    = tour_end
    ) # RETURN
}

# travel distance/time of individual salesmen/workers
# aux func to be called from other func
# paranoid checks should have been done in those other func
get_multi_tour_len <- function(
    chromosome,
    num_selected,
    tour_begin,     # vector
    tour_end,       # vector
    distance_matrix,
    start_from_spot
) {
    tour_sum <- rep(0, num_selected)
    for(tnd in 1L:num_selected) {
        if(chromosome[num_selected + tnd] > 0) {
            tour_sum[tnd] <- get_tour_len(
                chromosome      = chromosome,
                tour_begin      = tour_begin[tnd],
                tour_end        = tour_end[tnd],
                distance_matrix = distance_matrix,
                start_from_spot = start_from_spot
            )
        }
    }
    tour_sum # RETURN
}

# calibration cost of individual workers
# aux func to be called from other func
# paranoid checks should have been done in those other func
get_multi_cali_sum <- function(
    chromosome,
    num_selected,
    tour_begin,
    tour_end,
    spot_cali_cost
) {
    cali_sum <- rep(0, num_selected)
    for(tnd in 1L:num_selected) {
        if(chromosome[num_selected + tnd] > 0) {
            cali_sum[tnd] <- sum(
                spot_cali_cost[tour_begin[tnd]:tour_end[tnd]]
            )
        }
    }
    cali_sum # RETURN
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

        tour_begin_end <- get_tour_begin_end(chromosome, num_selected)
        tour_sum <- get_multi_tour_len(
            chromosome          = chromosome,
            num_selected        = num_selected,
            tour_begin          = tour_begin_end$tour_begin,
            tour_end            = tour_begin_end$tour_end,
            distance_matrix     = distance_matrix,
            start_from_spot     = start_from_spot
        )
        cali_sum <- rep(0, num_selected)
        if(is.finite(max_cost_worker)) {
            cali_sum <- get_multi_cali_sum(
                chromosome      = chromosome,
                num_selected    = num_selected,
                tour_begin      = tour_begin_end$tour_begin,
                tour_end        = tour_begin_end$tour_end,
                spot_cali_cost  = spot_cali_cost
            )
        }

        # check if any individual cost violates max cost per worker constraint
        # if(any(tour_sum + cali_sum > max_cost_worker)) {
        #     return(0)
        # }
        # will not do this check
        # will adjust solutions in a separate func instead

        # fitness value
        1 / (sum(tour_sum) + 1) # RETURN
    } # RETURN
}

get_compressed_chromosome <- function(
    chromosome,
    num_selected
) {
    # number of spots in each tour
    num_tour <- chromosome[(num_selected + 1L):(2L * num_selected)]
    valid_tours <- which(num_tour > 0)
    count_valid <- length(valid_tours)
    stopifnot(count_valid > 0)
    if(count_valid == num_selected) { return(chromosome) }

    num_tour[1L:count_valid] <- num_tour[valid_tours]
    num_tour[(count_valid + 1L):num_selected] <- 0
    chromosome[(num_selected + 1L):(2L * num_selected)] <- num_tour
    chromosome # RETURN
}

ga_adjust_chromosome <- function(
    chromosome,
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost = NULL,
    start_from_spot = 1L
) {
    num_selected <- as.integer(sum(l_selected > 0))
    chromosome <- get_compressed_chromosome(chromosome, num_selected)

    # repeat to split tours until constraints are met
    repeat{
        tour_num <- chromosome[(num_selected + 1L):(2L * num_selected)]
        tour_begin_end <- get_tour_begin_end(chromosome, num_selected)
        tour_sum <- get_multi_tour_len(
            chromosome          = chromosome,
            num_selected        = num_selected,
            tour_begin          = tour_begin_end$tour_begin,
            tour_end            = tour_begin_end$tour_end,
            distance_matrix     = distance_matrix,
            start_from_spot     = start_from_spot
        )
        cali_sum <- rep(0, num_selected)
        if(is.finite(max_cost_worker)) {
            cali_sum <- get_multi_cali_sum(
                chromosome      = chromosome,
                num_selected    = num_selected,
                tour_begin      = tour_begin_end$tour_begin,
                tour_end        = tour_begin_end$tour_end,
                spot_cali_cost  = spot_cali_cost
            )
        }

        # note that if a tour of length 1 still violates the constraint,
        #   nothing much can be done for this spot
        # this kind of case is considered "infeasible" using general solvers,
        #   but we still get a solution to meet as many constraints as possible
        constraint_met <- (tour_num <= 1 ||
            tour_sum + cali_sum <= max_cost_worker)
        if(all(constraint_met)) { break }

        tour_to_split <- which(!constraint_met)[1L]
        split_1 <- sample(x = 1:(tour_num[tour_to_split] - 1), size = 1L)
        split_2 <- tour_num[tour_to_split] - split_1
        tour_num[(tour_to_split + 2L):num_selected] <-
            tour_num[(tour_to_split + 1L):(num_selected - 1L)]
        tour_num[tour_to_split:(tour_to_split + 1L)] <- c(split_1, split_2)
        chromosome[(num_selected + 1L):(2L * num_selected)] <- tour_num
    }
    chromosome # RETURN
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
