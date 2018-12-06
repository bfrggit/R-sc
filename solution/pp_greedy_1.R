# pp_greedy_1.R
#
# Created: 2018-12-03
# Updated: 2018-12-05
#  Author: Charles Zhu
#
# mTSP (path planning) solver
# greedy solution

if(!exists("EX_PP_GREEDY_1_R")) {
    EX_PP_GREEDY_1_R <<- TRUE

source("lib/basic.R")
source("lib/graph_aux.R")

greedy_list_get_tour_len <- function(
    tour,
    l_selected,
    distance_matrix,
    start_from_spot,
    paranoid
) {
    if(paranoid) {
        stopifnot(is.integer(start_from_spot))
        stopifnot(length(start_from_spot) == 1L)
        stopifnot(start_from_spot > 0)

        stopifnot(is.vector(l_selected))
        stopifnot(is.integer(l_selected))
        stopifnot(all(l_selected == 0L | l_selected == 1L))
        stopifnot(l_selected[start_from_spot] == 0L)

        stop_if_not_valid_tour(tour, l_selected, start_from_spot)
    }

    tour_num <- length(tour)
    tour_sum <- 0

    # an empty tour has length 0
    if(tour_num < 1) { return(tour_sum) }

    # going from and back to depot is enough, if there is only one spot
    tour_sum <- tour_sum + distance_matrix[
        start_from_spot,
        tour[1L]
    ]
    tour_sum <- tour_sum + distance_matrix[
        tour[tour_num],
        start_from_spot
    ]
    if(tour_num == 1) { return(tour_sum) }

    # add distance between consecutive pairs of spots in the given tour
    for(j in 1L:(tour_num - 1L)) {
        tour_sum <- tour_sum + distance_matrix[
            tour[j],
            tour[j + 1L]
        ]
    }
    tour_sum # RETURN
}

# travel distance/time of individual salesmen/workers
# aux func to be called from other func
# paranoid checks should have been done in those other func
greedy_list_get_multi_tour_len <- function(
    tour_list,
    l_selected,
    distance_matrix,
    start_from_spot,
    paranoid
) {
    if(paranoid) {
        stopifnot(is.integer(start_from_spot))
        stopifnot(length(start_from_spot) == 1L)
        stopifnot(start_from_spot > 0)

        stopifnot(is.vector(l_selected))
        stopifnot(is.integer(l_selected))
        stopifnot(all(l_selected == 0L | l_selected == 1L))
        stopifnot(l_selected[start_from_spot] == 0L)

        stop_if_not_valid_tour_list(tour_list, l_selected, start_from_spot)
    }

    unlist(
        lapply(
            tour_list,
            function(tour) {
                greedy_list_get_tour_len(
                    tour            = tour,
                    l_selected      = l_selected,
                    distance_matrix = distance_matrix,
                    start_from_spot = start_from_spot,
                    paranoid        = paranoid
                )
            }
        )
    ) # RETURN
}

# calibration cost of individual workers
# aux func to be called from other func
# paranoid checks should have been done in those other func
greedy_list_get_multi_cali_sum <- function(
    tour_list,
    l_selected,
    spot_cali_cost,
    paranoid
) {
    if(paranoid) {
        stopifnot(is.vector(l_selected))
        stopifnot(is.integer(l_selected))
        stopifnot(all(l_selected == 0L | l_selected == 1L))

        stop_if_not_valid_tour_list(tour_list, l_selected, NULL)
    }

    unlist(
        lapply(
            tour_list,
            function(tour) {
                sum(spot_cali_cost[tour])
            }
        )
    ) # RETURN
}

get_multi_paths_greedy_1 <<- function(
    l_selected,             # selected spots, len L vector
    distance_matrix,        # distance matrix
    max_cost_worker = +Inf, # max workload of any single worker
    spot_cali_cost  = NULL, # per-spot calibration cost
    raw_return      = FALSE,
    paranoid        = TRUE  # enable/disable paranoid checks
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

    tour_list <- list()
    cost_sum <- numeric(0)
    which_selected <- which(l_selected == 1L)
    unvisited <- which_selected

    # loop until all spots are visited
    while(length(unvisited) > 0) {
        # compute the costs if a spot is added to a new worker
        tour_sum_new_w <- distance_matrix[
            1L, unvisited
        ] + distance_matrix[
            unvisited, 1L
        ]
        min_d_cost_new <- min(tour_sum_new_w)
        flag_new_worker <- TRUE
        to_visit <- NULL

        # if there are already workers that are deployed
        # i.e. this is not the first iteration in this loop
        if(length(tour_list) > 0) {
            # compute a matrix of travel distance increase to old workers
            # each row is an old worker, i.e. tour
            # each col is an unvisited spot
            d_tour_sum_old_w <- matrix(
                unlist(
                    lapply(
                        tour_list,
                        function(tour) {
                            last_spot <- tour[length(tour)]

                            distance_matrix[        # add last spot to new spot
                                last_spot, unvisited
                            ] + distance_matrix[    # add new spot to depot
                                unvisited, 1L
                            ] - distance_matrix[    # minus last spot to depot
                                last_spot, 1L
                            ] # RETURN
                        }
                    ) # this result in a list, whose length equals num of tours
                ),
                nrow = length(tour_list),
                ncol = length(unvisited),
                byrow = TRUE
            )

            # compute new cost for old workers to check constraint
            if(is.finite(max_cost_worker)) {
                new_cost_sum_old_w <- matrix(
                    rep(cost_sum, length(unvisited)),
                    nrow = length(tour_list),
                    ncol = length(unvisited),
                    byrow = FALSE
                ) + d_tour_sum_old_w + matrix(
                    rep(spot_cali_cost[unvisited], length(tour_list)),
                    nrow = length(tour_list),
                    ncol = length(unvisited),
                    byrow = TRUE
                )

                # in the case of constraint violation
                # set corresponding delta to +Inf to avoid selection
                d_tour_sum_old_w[
                    which(new_cost_sum_old_w > max_cost_worker)
                ] <- +Inf
            }

            # find the next spot to visit
            min_d_cost_old <- min(d_tour_sum_old_w)

            # if adding to an old worker is more efficient
            if(min_d_cost_old <= min_d_cost_new) {
                # find the minimum cost
                pair <- which(
                    d_tour_sum_old_w == min_d_cost_old,
                    arr.ind = TRUE
                )[1L, ]
                worker_to_load <- pair[1L]
                to_visit <- unvisited[pair[2L]]

                # modify the tours
                tour_list[[worker_to_load]][
                    length(tour_list[[worker_to_load]]) + 1L
                ] <- to_visit

                new_cost <- cost_sum[worker_to_load] + min_d_cost_old
                if(is.finite(max_cost_worker)) {
                    new_cost <- new_cost + spot_cali_cost[to_visit]
                }
                cost_sum[worker_to_load] <- new_cost
                flag_new_worker <- FALSE
            }
        }

        if(flag_new_worker) { # initial move
            to_visit <- unvisited[which(tour_sum_new_w == min_d_cost_new)[1L]]
            tour_list[[length(tour_list) + 1L]] <- c(to_visit)

            # compute the tour cost of the newly added worker
            new_cost <- min_d_cost_new
            if(is.finite(max_cost_worker)) {
                new_cost <- new_cost + spot_cali_cost[to_visit]
            }
            cost_sum[length(cost_sum) + 1L] <- new_cost
        }

        # remove the visited spot from list of unvisited spots
        if(paranoid) {
            stopifnot(sum(unvisited == to_visit) == 1)
        }

        unvisited <- unvisited[-which(unvisited == to_visit)]

        if(paranoid) {
            stopifnot(all(unvisited != to_visit))
            stopifnot(length(tour_list) == length(cost_sum))
        }
    }
    if(paranoid) {
        stop_if_not_valid_tour_list(
            tour_list       = tour_list,
            l_selected      = l_selected,
            start_from_spot = 1L
        )
    }
    if(raw_return) { return(tour_list) }

    num_workers <- length(tour_list)

    # construct solution data structure for my simulation framework
    paths_array <- array(
        data = rep(
            0L,
            NUM_SPOTS * NUM_SPOTS * num_workers
        ), dim = c(NUM_SPOTS, NUM_SPOTS, num_workers),
        dimnames = list(
            z_nd_str("spot", NUM_SPOTS),
            z_nd_str("spot", NUM_SPOTS),
            z_nd_str("worker", num_workers)
        )
    )

    for(worker in 1L:num_workers) {
        tour <- tour_list[[worker]]
        tour_len <- length(tour)

        if(paranoid) {
            stopifnot(length(tour) > 0)
        }
        paths_array[1L, tour[1L], worker] <- 1L
        paths_array[tour[tour_len], 1L, worker] <- 1L

        for(jnd in 1L:(tour_len - 1L)) {
            paths_array[
                tour[jnd],
                tour[jnd + 1L],
                worker
            ] <- 1L
        }
    }
    paths_array # RETURN
}

} # ENDIF
