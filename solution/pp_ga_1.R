# pp_ga_1.R
#
# Created: 2018-11-20
# Updated: 2018-11-27
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
    stopifnot(all(as.integer(chromosome) == chromosome))
    stopifnot(length(chromosome) > 1)
    stopifnot(length(chromosome) %% 2L == 0)

    num_selected <- length(chromosome) %/% 2L
    order_spots <- chromosome[1L:num_selected]
    stopifnot(all(order_spots >= 1))
    stopifnot(all(order_spots <= NUM_SPOTS))
    stopifnot(length(unique(order_spots)) == length(order_spots))
    stopifnot(all(order_spots != start_from_spot))

    num_tour <- chromosome[(num_selected + 1L):length(chromosome)]
    stopifnot(length(num_tour) == num_selected)
    stopifnot(all(num_tour >= 0))
    stopifnot(sum(num_tour) == num_selected)
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
    paranoid
) {
    if(paranoid) {
        stop_if_not_valid_chromosome(chromosome, start_from_spot)
        num_selected <- length(chromosome) %/% 2L

        stopifnot(tour_begin <= tour_end)
        # stopifnot(tour_begin <= num_selected)
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
    num_selected,
    paranoid
) {
    if(paranoid) {
        stop_if_not_valid_chromosome(chromosome)
        stopifnot(length(chromosome) == num_selected * 2L)
    }

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
    start_from_spot,
    paranoid
) {
    if(paranoid) {
        stop_if_not_valid_chromosome(chromosome, start_from_spot)
        stopifnot(length(chromosome) == num_selected * 2L)

        stopifnot(length(tour_begin) == num_selected)
        stopifnot(length(tour_end) == num_selected)
    }

    tour_sum <- rep(0, num_selected)
    for(tnd in 1L:num_selected) {
        if(chromosome[num_selected + tnd] > 0) {
            tour_sum[tnd] <- get_tour_len(
                chromosome      = chromosome,
                tour_begin      = tour_begin[tnd],
                tour_end        = tour_end[tnd],
                distance_matrix = distance_matrix,
                start_from_spot = start_from_spot,
                paranoid        = paranoid
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
            stop_if_not_valid_chromosome(chromosome, start_from_spot)
            stopifnot(length(chromosome) == num_selected * 2L)
        }

        tour_begin_end <- get_tour_begin_end(chromosome, num_selected, paranoid)
        tour_sum <- get_multi_tour_len(
            chromosome          = chromosome,
            num_selected        = num_selected,
            tour_begin          = tour_begin_end$tour_begin,
            tour_end            = tour_begin_end$tour_end,
            distance_matrix     = distance_matrix,
            start_from_spot     = start_from_spot,
            paranoid            = paranoid
        )
        cali_sum <- rep(0, num_selected)
        if(is.finite(max_cost_worker)) {
            cali_sum <- get_multi_cali_sum(
                chromosome      = chromosome,
                num_selected    = num_selected,
                tour_begin      = tour_begin_end$tour_begin,
                tour_end        = tour_begin_end$tour_end,
                spot_cali_cost  = spot_cali_cost,
                paranoid        = paranoid
            )
        }

        # check if any individual cost violates max cost per worker constraint
        # if(any(tour_sum + cali_sum > max_cost_worker)) {
        #     return(-Inf)
        # }
        # will not do this check
        # will adjust solutions in a separate func instead

        # fitness value
        -sum(tour_sum) # RETURN
    } # RETURN
}

# shift all zeros in num_tour (i.e. right half of chromosome) to the right
get_compressed_chromosome <- function(
    chromosome,
    num_selected,
    paranoid
) {
    if(paranoid) {
        stop_if_not_valid_chromosome(chromosome)
        stopifnot(length(chromosome) == num_selected * 2L)
    }

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
    chromosome <- get_compressed_chromosome(
        chromosome      = chromosome,
        num_selected    = num_selected,
        paranoid        = paranoid
    )

    # repeat to split tours until constraints are met
    repeat {
        if(paranoid) {
            stop_if_not_valid_chromosome(chromosome, start_from_spot)
            stopifnot(length(chromosome) == num_selected * 2L)
        }

        tour_num <- chromosome[(num_selected + 1L):(2L * num_selected)]
        tour_begin_end <- get_tour_begin_end(chromosome, num_selected, paranoid)
        tour_sum <- get_multi_tour_len(
            chromosome          = chromosome,
            num_selected        = num_selected,
            tour_begin          = tour_begin_end$tour_begin,
            tour_end            = tour_begin_end$tour_end,
            distance_matrix     = distance_matrix,
            start_from_spot     = start_from_spot,
            paranoid            = paranoid
        )
        cali_sum <- rep(0, num_selected)
        if(is.finite(max_cost_worker)) {
            cali_sum <- get_multi_cali_sum(
                chromosome      = chromosome,
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
        chromosome[(num_selected + 1L):(2L * num_selected)] <- tour_num
    }
    chromosome # RETURN
}

# used in the generation of initial population
get_random_chromosome <- function(
    l_selected
) {
    which_selected <- which(l_selected > 0)
    num_selected <- length(which_selected)
    order_spots <- sample(
        x = which_selected,
        size = num_selected,
        replace = FALSE
    )
    num_tour <- rep(0, num_selected)
    spots_left <- num_selected
    for(tnd in 1L:num_selected) {
        num_tour[tnd] <- sample(x = 1L:spots_left, size = 1L)
        spots_left <- spots_left - num_tour[tnd]
        if(spots_left < 1) { break }
    }
    chromosome <- c(order_spots, num_tour) # RETURN
}

# used as suggestion
get_naive_chromosome <- function(
    l_selected
) {
    which_selected <- which(l_selected > 0)
    num_selected <- length(which_selected)
    num_tour <- rep(1, num_selected)
    chromosome <- c(which_selected, num_tour) # RETURN
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
                chromosome      = get_random_chromosome(l_selected),
                l_selected      = l_selected,
                distance_matrix = distance_matrix,
                max_cost_worker = max_cost_worker,
                spot_cali_cost  = spot_cali_cost,
                start_from_spot = start_from_spot,
                paranoid        = paranoid
            )

            if(paranoid) {
                stop_if_not_valid_chromosome(chromosome, start_from_spot)
                stopifnot(length(chromosome) == num_selected * 2L)
            }

            population[pnd, ] <- chromosome
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
                )
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
refine_tour_three_opt <- function(
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

    # for each group of edges
    numeric(3L) -> nf -> nt -> ef -> et
    for(jnd in 1L:(num_edges_tour - 4L)) {
        nt[1L] <- tour_ref + jnd
        nf[1L] <- nt[1L] - 1L
        if(jnd == 1) {
            ef[1L] <- start_from_spot
        } else ef[1L] <- chromosome[nf[1L]]
        et[1L] <- chromosome[nt[1L]]

        kup <- num_edges_tour - 2L
        if(jnd == 1) { kup <- kup - 1L }
        for(knd in (jnd + 2L):kup) {
            nt[2L] <- tour_ref + knd
            nf[2L] <- nt[2L] - 1L
            ef[2L] <- chromosome[nf[2L]]
            et[2L] <- chromosome[nt[2L]]

            lup <- num_edges_tour
            if(jnd == 1) { lup <- lup - 1L }
            for(lnd in (knd + 2L):lup) {
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
                    return(chromosome)
                }
            }
        }
    }
    chromosome # RETURN
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

        flag_refine <- FALSE
        num_tour <- mutate[(num_selected + 1L):(2L * num_selected)]
        if(any(num_tour >= 6)) {
            flag_refine <- sample(c(FALSE, TRUE), size = 1L)
        }

        if(flag_refine) {
            tour_refine <- sample(x = which(num_tour >= 6), size = 1L)
            tour_begin_end <- get_tour_begin_end(
                chromosome      = mutate,
                num_selected    = num_selected,
                paranoid        = paranoid
            )
            mutate <- refine_tour_three_opt(
                chromosome      = mutate,
                tour_begin      = tour_begin_end$tour_begin[tour_refine],
                tour_end        = tour_begin_end$tour_end[tour_refine],
                distance_matrix = distance_matrix,
                start_from_spot = start_from_spot,
                paranoid        = paranoid
            )
            if(!paranoid) { return(mutate) }
        } else {
            flag_rev <- sample(c(FALSE, TRUE), size = 1L)
            if(flag_rev) {
                mutate[joints[2L]:joints[1L]] <-
                    mutate[joints[1L]:joints[2L]]
            } else {
                mutate[rev(joints)] <- mutate[joints]
            }
        }

        ga_adjust_chromosome(
            chromosome      = mutate,
            l_selected      = l_selected,
            distance_matrix = distance_matrix,
            max_cost_worker = max_cost_worker,
            spot_cali_cost  = spot_cali_cost,
            start_from_spot = start_from_spot,
            paranoid        = paranoid
        ) # RETURN
    } # RETURN
}

get_multi_paths_ga_1 <<- function(
    l_selected,             # selected spots, len L vector
    distance_matrix,        # distance matrix
    max_cost_worker = +Inf, # max workload of any single worker
    spot_cali_cost  = NULL, # per-spot calibration cost
    ga_seed         = NULL,
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

    num_selected <- as.integer(sum(l_selected > 0))

    ga_pop_size <- 50L
    ga_max_iter <- 10L * num_selected
    ga_run <- 3L * num_selected
    ga_obj <- ga(
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
        pmutation = 0.2,        # default value = 0.1
        elitism = round(0.05 * ga_pop_size),
        updatePop = FALSE,      # do not use this experimental feature for now
        maxiter = ga_max_iter,  # default value = 100
        run = ga_run,           # default value = maxiter
        maxFitness = 0,
        names = NULL,
        suggestions = get_naive_chromosome(l_selected),
        parallel = FALSE,
        # monitor = ga_monitor_f,
        seed = ga_seed
    )

    ga_solution <- NULL
    if(is.matrix(ga_obj@solution)) {
        ga_solution <- ga_obj@solution[1L, ]
    } else {
        ga_solution <- ga_obj@solution
    }

    if(paranoid) {
        ga_solution <- get_compressed_chromosome(
            chromosome      = ga_solution,
            num_selected    = num_selected,
            paranoid        = paranoid
        )
    }
    num_tour <- ga_solution[(num_selected + 1L):(2L * num_selected)]
    workers <- which(num_tour > 0)
    num_workers <- length(workers)
    tour_begin_end <- get_tour_begin_end(
        chromosome      = ga_solution,
        num_selected    = num_selected,
        paranoid        = paranoid
    )

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
