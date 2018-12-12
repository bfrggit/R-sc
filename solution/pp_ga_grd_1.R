# pp_ga_grd_1.R
#
# Created: 2018-12-03
# Updated: 2018-12-11
#  Author: Charles Zhu
#
# mTSP (path planning) solver
# GA solution, single chromosome, with greedy init suggestion
# some paranoid checks have been removed to speed up even in paranoid mode
#
# References:
#   Mohammad Sedighpour, Majid Yousefikhoshbakht, and Narges Mahmoodi Darani.
#       "An effective genetic algorithm for solving
#           the multiple traveling salesman problem."
#       Journal of Optimization in Industrial Engineering 8 (2012): 73-79.
#       http://www.qjie.ir/article_89_0.html

if(!exists("EX_PP_GA_GRD_1_R")) {
    EX_PP_GA_GRD_1_R <<- TRUE

source("lib/basic.R")
source("lib/ga_debug.R")
source("solution/pp_each.R")
source("solution/pp_greedy_1.R")

suppressPackageStartupMessages(library(GA))

stop_if_not_valid_chromosome <- function(
    chromosome,
    start_from_spot = NULL
) {
    if(!is.null(start_from_spot)) {
        stopifnot(is.integer(start_from_spot))
        stopifnot(length(start_from_spot) == 1L)
        stopifnot(start_from_spot > 0)
    }

    stopifnot(is.vector(chromosome))
    stopifnot(is.numeric(chromosome))
    stopifnot(length(chromosome) > 1)
    stopifnot(length(chromosome) %% 2L == 0)
    stopifnot(all(is.finite(chromosome)))
    stopifnot(all(as.integer(chromosome) == chromosome))

    num_selected <- length(chromosome) %/% 2L
    order_spots <- chromosome[1L:num_selected]
    stopifnot(all(order_spots >= 1))
    stopifnot(all(order_spots <= NUM_SPOTS))
    stopifnot(length(unique(order_spots)) == length(order_spots))
    stopifnot(all(order_spots != start_from_spot))

    tour_num <- chromosome[-(1L:num_selected)]
    stopifnot(length(tour_num) == num_selected)
    stopifnot(all(tour_num >= 0))
    stopifnot(sum(tour_num) == num_selected)
}

ga_get_tour_len <- function(
    chromosome,
    tour_begin,
    tour_end,
    distance_matrix,
    start_from_spot
) {
    tour_num <- tour_end - tour_begin + 1L # number of non-depot spots
    tour_sum <- 0

    # an empty tour has length 0
    if(tour_num < 1) { return(tour_sum) }

    # going from and back to depot is enough, if there is only one spot
    tour_sum <- tour_sum + distance_matrix[
        start_from_spot,
        chromosome[tour_begin]
    ]
    tour_sum <- tour_sum + distance_matrix[
        chromosome[tour_end],
        start_from_spot
    ]
    if(tour_num == 1) { return(tour_sum) }

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
ga_get_tour_range <- function(
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
ga_get_multi_tour_len <- function(
    chromosome,
    num_selected,
    tour_begin,     # vector
    tour_end,       # vector
    distance_matrix,
    start_from_spot,
    paranoid
) {
    if(paranoid) {
        # stop_if_not_valid_chromosome(chromosome, start_from_spot)
        # stopifnot(length(chromosome) == num_selected * 2L)
        #
        stopifnot(length(tour_begin) == num_selected)
        stopifnot(length(tour_end) == num_selected)
    }

    tour_sum <- rep(0, num_selected)
    for(tnd in 1L:num_selected) {
        if(chromosome[num_selected + tnd] > 0) {
            tour_sum[tnd] <- ga_get_tour_len(
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
ga_get_multi_cali_sum <- function(
    chromosome,
    num_selected,
    tour_begin,
    tour_end,
    spot_cali_cost,
    paranoid
) {
    if(paranoid) {
        stop_if_not_valid_chromosome(chromosome)
        stopifnot(length(chromosome) == num_selected * 2L)

        stopifnot(tour_end <= num_selected)
    }

    cali_sum <- rep(0, num_selected)
    for(tnd in 1L:num_selected) {
        if(chromosome[num_selected + tnd] > 0) {
            cali_sum[tnd] <- sum(
                spot_cali_cost[chromosome[tour_begin[tnd]:tour_end[tnd]]]
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
            stop_if_not_valid_chromosome(chromosome, start_from_spot)
            stopifnot(length(chromosome) == num_selected * 2L)
        }

        tour_begin_end <- ga_get_tour_range(chromosome, num_selected)
        tour_sum <- ga_get_multi_tour_len(
            chromosome          = chromosome,
            num_selected        = num_selected,
            tour_begin          = tour_begin_end$tour_begin,
            tour_end            = tour_begin_end$tour_end,
            distance_matrix     = distance_matrix,
            start_from_spot     = start_from_spot,
            paranoid            = paranoid
        )
        fitness <- 0 - sum(tour_sum)

        cali_sum <- rep(0, num_selected)
        if(is.finite(max_cost_worker)) {
            cali_sum <- ga_get_multi_cali_sum(
                chromosome      = chromosome,
                num_selected    = num_selected,
                tour_begin      = tour_begin_end$tour_begin,
                tour_end        = tour_begin_end$tour_end,
                spot_cali_cost  = spot_cali_cost,
                paranoid        = paranoid
            )
            constraint_met <- (tour_sum + cali_sum <= max_cost_worker)
            fitness <- fitness - 1000000 * sum(!constraint_met)
        }

        # fitness value
        fitness # RETURN
    } # RETURN
}

# shift all zeros in tour_num (i.e. right half of chromosome) to the right
ga_compress_chromosome <- function(
    chromosome,
    num_selected,
    paranoid
) {
    if(paranoid) {
        stop_if_not_valid_chromosome(chromosome)
        stopifnot(length(chromosome) == num_selected * 2L)
    }

    # number of spots in each tour
    tour_num <- chromosome[-(1L:num_selected)]
    valid_tours <- which(tour_num > 0)
    count_valid <- length(valid_tours)
    # stopifnot(count_valid > 0)
    if(count_valid == num_selected) { return(chromosome) }

    tour_num[1L:count_valid] <- tour_num[valid_tours]
    tour_num[-(1L:count_valid)] <- 0
    chromosome[-(1L:num_selected)] <- tour_num
    chromosome # RETURN
}

# split tours that violate the workload constraint
ga_adjust_chromosome <- function(
    chromosome,
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost  = NULL,
    start_from_spot = 1L,
    paranoid
) {
    if(paranoid) {
        stopifnot(is.infinite(max_cost_worker) || is.vector(spot_cali_cost))
    }

    num_selected <- as.integer(sum(l_selected > 0))
    adjusted <- ga_compress_chromosome(
        chromosome      = chromosome,
        num_selected    = num_selected,
        paranoid        = paranoid
    )

    # repeat to split tours until constraints are met
    repeat {
        # a lot of tests have been done here
        # it never fails in this loop thus paranoid check is disabled here
        #   to speed up even in paranoid mode
        #
        # if(paranoid) {
        #     stop_if_not_valid_chromosome(adjusted, start_from_spot)
        #     stopifnot(length(adjusted) == num_selected * 2L)
        # }
        #
        tour_num <- adjusted[-(1L:num_selected)]
        tour_begin_end <- ga_get_tour_range(adjusted, num_selected)
        tour_sum <- ga_get_multi_tour_len(
            chromosome          = adjusted,
            num_selected        = num_selected,
            tour_begin          = tour_begin_end$tour_begin,
            tour_end            = tour_begin_end$tour_end,
            distance_matrix     = distance_matrix,
            start_from_spot     = start_from_spot,
            paranoid            = paranoid
        )
        cali_sum <- rep(0, num_selected)
        if(is.finite(max_cost_worker)) {
            cali_sum <- ga_get_multi_cali_sum(
                chromosome      = adjusted,
                num_selected    = num_selected,
                tour_begin      = tour_begin_end$tour_begin,
                tour_end        = tour_begin_end$tour_end,
                spot_cali_cost  = spot_cali_cost,
                paranoid        = paranoid
            )
        }

        # note that if a tour of length 1 still violates the constraint,
        #   nothing much can be done for this spot
        # this kind of case is considered "infeasible" using general solvers,
        #   but we still get a solution to meet as many constraints as possible
        constraint_met <- (tour_num <= 1 |
            tour_sum + cali_sum <= max_cost_worker)
        if(all(constraint_met)) { break }

        if(paranoid) {
            stopifnot(tour_num[num_selected] == 0)
        }

        # by doing this split we are assuming the triangule inequality holds
        # so that going from/back to the depot directly is always fater than
        #   taking some additional routes, which guarantees
        #   the resulting two tours are both shorter than the tour we split
        # note that the two tours together will also be no shorter than the
        #   tour we split, suggested by the triangle inequality
        tour_to_split <- which(!constraint_met)[1L]
        split_1 <- sample(x = 1:(tour_num[tour_to_split] - 1), size = 1L)
        split_2 <- tour_num[tour_to_split] - split_1
        if(tour_to_split + 2L <= num_selected) {
            indexes_after_split <- (tour_to_split + 1L):(num_selected - 1L)
            tour_num[indexes_after_split + 1L] <- tour_num[indexes_after_split]
        }
        tour_num[tour_to_split:(tour_to_split + 1L)] <- c(split_1, split_2)

        if(paranoid) {
            stopifnot(length(tour_num) == num_selected)
        }

        # update chromosome
        adjusted[-(1L:num_selected)] <- tour_num
    }
    adjusted # RETURN
}

# used in the generation of initial population
ga_get_random_chromosome <- function(
    l_selected
) {
    which_selected <- which(l_selected > 0)
    num_selected <- length(which_selected)
    order_spots <- sample(
        x = which_selected,
        size = num_selected,
        replace = FALSE
    )
    tour_num <- rep(0, num_selected)
    spots_left <- num_selected
    for(tnd in 1L:num_selected) {
        tour_num[tnd] <- sample(x = 1L:spots_left, size = 1L)
        spots_left <- spots_left - tour_num[tnd]
        if(spots_left < 1) { break }
    }
    chromosome <- c(order_spots, tour_num) # RETURN
}

# used as suggestion
ga_get_naive_chromosome <- function(
    l_selected
) {
    which_selected <- which(l_selected > 0)
    num_selected <- length(which_selected)
    tour_num <- rep(1, num_selected)
    chromosome <- c(which_selected, tour_num) # RETURN
}

get_population_f_ga_1 <- function(
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost  = NULL,
    start_from_spot = 1L,
    paranoid        = TRUE
) {
    num_selected <- as.integer(sum(l_selected > 0))

    function(object) {
        population <- matrix(
            numeric(object@popSize * num_selected * 2L),
            nrow = object@popSize,
            ncol = num_selected * 2L
        )

        # actually generate each chromosome
        for(pnd in 1L:object@popSize) {
            chromosome <- ga_adjust_chromosome(
                chromosome      = ga_get_random_chromosome(l_selected),
                l_selected      = l_selected,
                distance_matrix = distance_matrix,
                max_cost_worker = max_cost_worker,
                spot_cali_cost  = spot_cali_cost,
                start_from_spot = start_from_spot,
                paranoid        = paranoid
            )
            population[pnd, ] <- chromosome

            if(paranoid) {
                stop_if_not_valid_chromosome(
                    population[pnd, ],
                    start_from_spot
                )
            }
        }

        population # RETURN
    } # RETURN
}

get_crossover_f_ga_1 <- function(
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost  = NULL,
    start_from_spot = 1L,
    paranoid        = TRUE
) {
    num_selected <- as.integer(sum(l_selected > 0))

    function(object, parents) {
        if(paranoid) {
            stopifnot(length(parents) == 2)

            stop_if_not_valid_chromosome(
                object@population[parents[1L], ],
                start_from_spot
            )
            stop_if_not_valid_chromosome(
                object@population[parents[2L], ],
                start_from_spot
            )
        }

        children <- matrix(
            numeric(num_selected * 4L),
            nrow = 2L,
            ncol = num_selected * 2L
        )

        for(pnd in 1L:2L) {
            pnd_other <- 3L - pnd
            cross_genes <- sort(
                sample(
                    x = 1L:num_selected,
                    size = sample(
                        x = 1L:num_selected,
                        size = 1L
                    )
                ),
                method = "quick"
            )
            cross_gene_values <- object@population[parents[pnd], cross_genes]
            other_gene_values <- object@population[
                parents[pnd_other],
                which(
                    object@population[
                        parents[pnd_other],
                        1L:num_selected
                    ] %in% cross_gene_values
                )
            ]

            child <- object@population[parents[pnd], ]
            child[cross_genes] <- other_gene_values
            children[pnd, ] <- ga_adjust_chromosome(
                chromosome      = child,
                l_selected      = l_selected,
                distance_matrix = distance_matrix,
                max_cost_worker = max_cost_worker,
                spot_cali_cost  = spot_cali_cost,
                start_from_spot = start_from_spot,
                paranoid        = paranoid
            )
        }

        if(paranoid) {
            stop_if_not_valid_chromosome(children[1L, ], start_from_spot)
            stop_if_not_valid_chromosome(children[2L, ], start_from_spot)
        }

        list(
            children    = children,
            fitness     = rep(NA, 2L)
        ) # RETURN
    } # RETURN
}

# use the three-opt local search to refine a single tour
ga_refine_tour_three_opt <- function(
    chromosome,
    tour_begin,
    tour_end,
    distance_matrix,
    start_from_spot,
    paranoid
) {
    if(paranoid) {
        stop_if_not_valid_chromosome(chromosome, start_from_spot)
    }
    num_edges_tour <- tour_end - tour_begin + 2L
    tour_ref <- tour_begin - 1L

    # we need at least 6 edges to use the three-opt local search
    if(num_edges_tour < 6) { return(chromosome) }

    last_updated_edges <- rep(0L, 3L)
    repeat {
        flag_update <- FALSE

        # nf, nt: where to find the indexes of from/to spots in the chromosome
        #         can be computed easily from edge ids (i.e. jnd, knd, lnd)
        # ef, et: actual indexes of from/to spots
        numeric(3L) -> nf -> nt -> ef -> et

        # for each group of three edges (using nested for loops)
        for(jnd in 1L:(num_edges_tour - 4L)) {
            # find the spots at the ends of the edges
            nt[1L] <- tour_ref + jnd
            nf[1L] <- nt[1L] - 1L
            if(jnd == 1) {
                ef[1L] <- start_from_spot
            } else ef[1L] <- chromosome[nf[1L]]
            et[1L] <- chromosome[nt[1L]]

            # find the range of k to search
            kup <- num_edges_tour - 2L
            if(jnd == 1) { kup <- kup - 1L }
            krange <- (jnd + 2L):kup

            for(knd in krange) {
                # find the spots at the ends of the edges
                nt[2L] <- tour_ref + knd
                nf[2L] <- nt[2L] - 1L
                ef[2L] <- chromosome[nf[2L]]
                et[2L] <- chromosome[nt[2L]]

                # find the range of l to search
                lup <- num_edges_tour
                if(jnd == 1) { lup <- lup - 1L }
                lrange <- (knd + 2L):lup

                # try to avoid searched combinations
                if(all(last_updated_edges > 0)) {
                    if(jnd < last_updated_edges[1L]) {
                        if(!(knd %in% last_updated_edges)) {
                            lrange <- last_updated_edges[
                                which(last_updated_edges %in% lrange)
                            ]
                        }
                    }
                }

                for(lnd in lrange) {
                    # find the spots at the ends of the edges
                    nt[3L] <- tour_ref + lnd
                    nf[3L] <- nt[3L] - 1L
                    ef[3L] <- chromosome[nf[3L]]
                    if(lnd == num_edges_tour) {
                        et[3L] <- start_from_spot
                    } else et[3L] <- chromosome[nt[3L]]

                    sum_old <- 0
                    sum_new <- 0
                    for(selected_edge in 1L:3L) {
                        sum_old <- sum_old + distance_matrix[
                            ef[selected_edge],
                            et[selected_edge]
                        ]
                        sum_new <- sum_new + distance_matrix[
                            ef[selected_edge],
                            et[selected_edge %% 3L + 1L]
                        ]
                    }

                    if(sum_new < sum_old) {
                        # reorder chromosome
                        # -1 bg +1 +2 f1 t1 +1 +2 +3 f2 t2 +1 f3 t3 +1 +2 nd +1
                        # sf----------|   |----------|   |----|   |----------sf
                        # old:        A              B        C               D
                        # new:        A              C        B               D
                        #
                        # example:
                        #   bg == 1, nd == 5, chromosome == c(2, 3, 4, 5, 6)
                        #   nf == c(0, 2, 4)
                        #   nt == c(1, 3, 5)
                        #   chromosome[1:4] <- c(
                        #       chromosome[3:4],
                        #       chromosome[1:2]
                        #   ), which means chromosome <- c(4, 5, 2, 3, 6)
                        #
                        chromosome[nt[1L]:nf[3L]] <- c(
                            chromosome[nt[2L]:nf[3L]],
                            chromosome[nt[1L]:nf[2L]]
                        )
                        flag_update <- TRUE
                        break
                    }
                }
                if(flag_update) { break }
            }
            if(flag_update) { break }
        }
        if(!flag_update) { break }
    }
    chromosome # RETURN
}

# merge short tours to create a valid long tour
ga_merge_tours <- function(
    chromosome,
    num_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost,
    start_from_spot,
    paranoid
) {
    if(paranoid) {
        stopifnot(is.integer(num_selected))
        stopifnot(length(num_selected) == 1)
    }

    merged <- chromosome
    repeat {
        # a lot of tests have been done here
        # it never fails in this loop thus paranoid check is disabled here
        #   to speed up even in paranoid mode
        #
        # if(paranoid) {
        #     stop_if_not_valid_chromosome(merged, start_from_spot)
        # }
        #
        tour_begin_end <- ga_get_tour_range(merged, num_selected)
        tour_sum <- ga_get_multi_tour_len(
            chromosome          = merged,
            num_selected        = num_selected,
            tour_begin          = tour_begin_end$tour_begin,
            tour_end            = tour_begin_end$tour_end,
            distance_matrix     = distance_matrix,
            start_from_spot     = start_from_spot,
            paranoid            = paranoid
        )
        cali_sum <- rep(0, num_selected)
        if(is.finite(max_cost_worker)) {
            cali_sum <- ga_get_multi_cali_sum(
                chromosome      = merged,
                num_selected    = num_selected,
                tour_begin      = tour_begin_end$tour_begin,
                tour_end        = tour_begin_end$tour_end,
                spot_cali_cost  = spot_cali_cost,
                paranoid        = paranoid
            )
        }
        cost_sum <- tour_sum + cali_sum # total cost for each worker

        tour_num <- merged[-(1L:num_selected)]
        valid_tours <- which(tour_num > 0)
        count_valid <- length(valid_tours)
        if(count_valid < 2) { break }

        flag_update <- FALSE

        # ns, nl: where to find indexes of begin/end of each tour in chromosome
        # es, el: actual indexes of start/last spots in each tour,
        #         excluding the depot
        numeric(2L) -> ns -> nl -> es -> el

        # for each pair of tours
        for(jnd in 1L:(count_valid - 1L)) {
            tour_j <- valid_tours[jnd]
            ns[1L] <- tour_begin_end$tour_begin[tour_j]
            nl[1L] <- tour_begin_end$tour_end[tour_j]
            es[1L] <- merged[ns[1L]]
            el[1L] <- merged[nl[1L]]

            for(knd in (jnd + 1L):count_valid) {
                tour_k <- valid_tours[knd]
                ns[2L] <- tour_begin_end$tour_begin[tour_k]
                nl[2L] <- tour_begin_end$tour_end[tour_k]
                es[2L] <- merged[ns[2L]]
                el[2L] <- merged[nl[2L]]

                cost_sum_both <- cost_sum[tour_j] + cost_sum[tour_k]
                d_cost <- map_graph_distances[el[1L], es[2L]] -
                    map_graph_distances[el[1L], start_from_spot] -
                    map_graph_distances[start_from_spot, es[2L]]

                if(d_cost <= 0 && cost_sum_both + d_cost <= max_cost_worker) {
                    if(jnd + 1L < knd) {
                        if(paranoid) {
                            stopifnot(ns[2L] - nl[1L] > 1)
                        }

                        # swap affected section with the spots in tour k
                        # sj +1 +2 +3 lj +1 +2 +3 +4 +5 sk +1 +2 +3 +4 lk
                        #                 |-----------|  |--------------|
                        tour_k_len <- nl[2L] - ns[2L] + 1L
                        affected_sec <- merged[(nl[1L] + 1L):(ns[2L] - 1L)]
                        merged[(nl[1L] + 1L):(nl[1L] + tour_k_len)] <-
                            merged[ns[2L]:nl[2L]]
                        merged[(nl[1L] + tour_k_len + 1L):nl[2L]] <-
                            affected_sec
                    }
                    tour_num[tour_j] <- tour_num[tour_j] + tour_num[tour_k]
                    tour_num[tour_k] <- 0
                    merged[-(1L:num_selected)] <- tour_num

                    flag_update <- TRUE
                    break
                }
            }
            if(flag_update) { break }
        }
        if(!flag_update) { break }
    }
    merged # RETURN
}

get_mutation_f_ga_1 <- function(
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost  = NULL,
    start_from_spot = 1L,
    paranoid        = TRUE
) {
    num_selected <- as.integer(sum(l_selected > 0))

    function(object, parent) {
        if(paranoid) {
            stop_if_not_valid_chromosome(
                object@population[parent, ],
                start_from_spot
            )
        }

        joints <- sort(sample(x = 1L:num_selected, size = 2L, replace = FALSE))
        mutate <- object@population[parent, ]
        mutate_modes <- c("switch", "reverse")

        tour_num <- mutate[-(1L:num_selected)]
        if(any(tour_num >= 6)) {
            # if there is a long tour for any worker
            # we may want to refine it with a local search (see below)
            # now the probability to conduct local search is hard coded as 1/2
            mutate_modes <- c(mutate_modes, "refine")
        }
        if(sum(tour_num > 0) > 1) {
            mutate_modes <- c(mutate_modes, "merge")
        }

        mutate_mode <- sample(x = mutate_modes, size = 1L)
        if(mutate_mode == "refine") {
            tour_refine <- sample(x = which(tour_num >= 6), size = 1L)
            tour_begin_end <- ga_get_tour_range(mutate, num_selected)
            mutate <- ga_refine_tour_three_opt(
                chromosome      = mutate,
                tour_begin      = tour_begin_end$tour_begin[tour_refine],
                tour_end        = tour_begin_end$tour_end[tour_refine],
                distance_matrix = distance_matrix,
                start_from_spot = start_from_spot,
                paranoid        = paranoid
            )
        } else if(mutate_mode == "merge") {
            mutate <- ga_merge_tours(
                chromosome      = mutate,
                num_selected    = num_selected,
                distance_matrix = distance_matrix,
                max_cost_worker = max_cost_worker,
                spot_cali_cost  = spot_cali_cost,
                start_from_spot = start_from_spot,
                paranoid        = paranoid
            )
        } else if(mutate_mode == "reverse") {
                mutate[joints[2L]:joints[1L]] <-
                    mutate[joints[1L]:joints[2L]]
        } else if(mutate_mode == "switch") {
            mutate[rev(joints)] <- mutate[joints]
        } else {
            stop()
        }

        mutate <- ga_adjust_chromosome(
            chromosome      = mutate,
            l_selected      = l_selected,
            distance_matrix = distance_matrix,
            max_cost_worker = max_cost_worker,
            spot_cali_cost  = spot_cali_cost,
            start_from_spot = start_from_spot,
            paranoid        = paranoid
        )

        if(paranoid) {
            stop_if_not_valid_chromosome(mutate, start_from_spot)
        }

        mutate# RETURN
    } # RETURN
}

#---------------------------------------------------------------------------
# primary functional component
# simulator calls this function to compute the solution
# this function uses GA to solve the problem
#
get_multi_paths_ga_grd_1 <<- function(
    l_selected,             # selected spots, len L vector
    distance_matrix,        # distance matrix
    max_cost_worker = +Inf, # max workload of any single worker
    spot_cali_cost  = NULL, # per-spot calibration cost
    ga_seed         = 9L,
    greedy_init     = TRUE, # enable/disable greedy suggestion for init pop
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

    vec_suggestion <- naive_suggestion <- ga_get_naive_chromosome(l_selected)
    num_selected <- as.integer(sum(l_selected > 0))

    # if there is only one spot to visit
    # the solution will be obvious, which is to send one worker to visit it
    # also many aux func of the GA solution will not work
    # hence we immediately return a solutoin generated by pp_each
    if(num_selected <= 1) {
        return(
            get_multi_paths_each(
                l_selected  = l_selected,
                paranoid    = paranoid
            )
        )
    }

    # make initial suggestion with greedy planning
    if(greedy_init) {
        greedy_tour_list <- get_multi_paths_greedy_1(
            l_selected      = l_selected,
            distance_matrix = distance_matrix,
            max_cost_worker = max_cost_worker,
            spot_cali_cost  = spot_cali_cost,
            raw_return      = TRUE,
            paranoid        = paranoid
        )
        greedy_order_spots <- unlist(greedy_tour_list)
        greedy_tour_num <- unlist(
            lapply(
                greedy_tour_list,
                function(tour) {
                    length(tour) # RETURN
                }
            )
        )

        if(paranoid) {
            stopifnot(length(greedy_order_spots) == num_selected)
            stopifnot(length(greedy_tour_num) <= num_selected)
        }

        if(length(greedy_tour_num) < num_selected) {
            greedy_tour_num <- c(
                greedy_tour_num, rep(
                    0,
                    num_selected - length(greedy_tour_num)
                )
            )
        }
        greedy_suggestion <- c(greedy_order_spots, greedy_tour_num)
        vec_suggestion <- c(vec_suggestion, greedy_suggestion)
    }

    ga_suggestions <- matrix(
        vec_suggestion,
        ncol = 2L * num_selected,
        byrow = TRUE
    )

    if(paranoid) {
        stop_if_not_valid_chromosome(ga_suggestions[1L, ], 1L)
        stop_if_not_valid_chromosome(ga_suggestions[2L, ], 1L)
    }

    ga_pop_size <- 100L
    ga_max_iter <- 100L + 10L * num_selected
    ga_run <- 20L + num_selected
    ga_obj <- ga_debug(
        type = "real-valued",
        fitness = get_fitness_f_ga_1(
            l_selected      = l_selected,
            distance_matrix = distance_matrix,
            max_cost_worker = max_cost_worker,
            spot_cali_cost  = spot_cali_cost,
            start_from_spot = 1L,
            paranoid        = paranoid
        ),
        population = get_population_f_ga_1(
            l_selected      = l_selected,
            distance_matrix = distance_matrix,
            max_cost_worker = max_cost_worker,
            spot_cali_cost  = spot_cali_cost,
            start_from_spot = 1L,
            paranoid        = paranoid
        ),
        crossover = get_crossover_f_ga_1(
            l_selected      = l_selected,
            distance_matrix = distance_matrix,
            max_cost_worker = max_cost_worker,
            spot_cali_cost  = spot_cali_cost,
            start_from_spot = 1L,
            paranoid        = paranoid
        ),
        mutation = get_mutation_f_ga_1(
            l_selected      = l_selected,
            distance_matrix = distance_matrix,
            max_cost_worker = max_cost_worker,
            spot_cali_cost  = spot_cali_cost,
            start_from_spot = 1L,
            paranoid        = paranoid
        ),
        # min = # deprecated
        # max = # deprecated
        lower = c(
            rep(1, num_selected),   # lower bound of spot number
            rep(0, num_selected)    # lower bound of num spots in tour
        ),
        upper = c(
            rep(NUM_SPOTS, num_selected),
            rep(NUM_SPOTS, num_selected)
        ),
        popSize = ga_pop_size,  # default value = 50
        pcrossover = 0.8,       # default value = 0.8
        pmutation = 0.1,        # default value = 0.1
        elitism = round(0.05 * ga_pop_size),
        updatePop = FALSE,      # do not use this experimental feature for now
        maxiter = ga_max_iter,  # default value = 100
        run = ga_run,           # default value = maxiter
        maxFitness = 0,
        names = NULL,
        suggestions = ga_suggestions,
        parallel = FALSE,
        # monitor = ga_monitor_f,
        seed = ga_seed
    )

    ga_solution <- NULL
    # ga_fitness <<- NULL
    if(is.matrix(ga_obj@solution)) {
        ga_solution <- ga_obj@solution[1L, ]
        # ga_fitness <<- ga_obj@fitnessValue[1L]
    } else {
        ga_solution <- ga_obj@solution
        # ga_fitness <<- ga_obj@fitnessValue
    }

    if(paranoid) {
        ga_solution <- ga_compress_chromosome(
            chromosome      = ga_solution,
            num_selected    = num_selected,
            paranoid        = paranoid
        )
    }
    tour_num <- ga_solution[-(1L:num_selected)]
    workers <- which(tour_num > 0)
    num_workers <- length(workers)
    tour_begin_end <- ga_get_tour_range(ga_solution, num_selected)

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
        tour_begin <- tour_begin_end$tour_begin[workers[worker]]
        tour_end <- tour_begin_end$tour_end[workers[worker]]

        if(paranoid) {
            stopifnot(tour_begin <= tour_end)
        }

        num_edges_tour <- tour_end - tour_begin + 2L
        for(jnd in 1L:num_edges_tour) {
            NULL -> fs -> ts
            if(jnd == 1) {
                fs <- 1L
            } else fs <- ga_solution[tour_begin + jnd - 2L]
            if(jnd == num_edges_tour) {
                ts <- 1L
            } else ts <- ga_solution[tour_begin + jnd - 1L]
            paths_array[fs, ts, worker] <- 1L
        }
    }
    paths_array # RETURN
}

} # ENDIF

# non-reachable section
# used for manual tests
if(FALSE) {

rm(list = ls())
load("t_pp/prep_RData/graph_60_4.RData")

paths_array <- get_multi_paths_ga_grd_1(
  l_selected = c(0L, rep(1L, NUM_SPOTS - 1L)),
  distance_matrix = map_graph_distances,
  max_cost_worker = 3600,
  spot_cali_cost = c(0L, rep(300L, NUM_SPOTS - 1L)),
  ga_seed = 9L,
  paranoid = FALSE
)

}
