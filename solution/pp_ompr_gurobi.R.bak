# pp_ompr_gurobi.R
#
# Created: 2018-11-12
# Updated: 2018-11-20
#  Author: Charles Zhu
#
# mTSP (path planning) solver
# MILP solution using OMPR and Gurobi
#
# Examples:
#   Dirk Schumacher, The Multiple Traveling Salesmen Problem:
#       https://dirkschumacher.github.io/ompr/articles/problem-mtsp.html
#
# References:
#   OMPR: https://dirkschumacher.github.io/ompr/index.html
#   Gurobi: http://www.gurobi.com/
#   ROI.plugin.gurobi: https://github.com/Fl0Sch/ROI.plugin.gurobi/

if(!exists("EX_PP_OMPR_GUROBI_R")) {
    EX_PP_OMPR_GUROBI_R <<- TRUE

library(magrittr)
suppressPackageStartupMessages(library(dplyr))
library(ompr)
library(ompr.roi)
library(ROI.plugin.gurobi)

source("lib/basic.R")

get_multi_paths_ompr_gurobi <<- function(
    l_selected,             # selected spots, len L vector
    distance_matrix,        # distance matrix
    num_workers = NULL,     # number of workers (a.k.a. salesmen)
    fix_workers = FALSE,    # if solution must contain the exact num of workers
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

        if(is.integer(num_workers)) {
            stopifnot(length(num_workers) == 1L)
            stopifnot(num_workers > 0L)
            stopifnot(num_workers < NUM_SPOTS)
        } else {
            # num_workers must be
            #   integer, OR
            #   NULL (i.e. not defined)
            stopifnot(is.null(num_workers))
        }

        # either selected spot vec or num of workers should be defined
        # stopifnot(!is.null(num_workers) || !is.null(l_selected))

        stopifnot(is.logical(fix_workers))
        stopifnot(length(fix_workers) == 1L)
        if(fix_workers) {
            stopifnot(is.integer(num_workers))
        }

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

    if(is.null(num_workers)) {
        num_workers <- length(which(l_selected > 0))
    }

    # allow a salesman to stay at depot if we do not set exact num of workers
    same_city_constraint_applies_to <- switch(
        fix_workers + 1L,
        2L,
        1L
    ):NUM_SPOTS

    model <- MIPModel() %>%

    # we create a variable that is 1 iff salesman k travels from city i to j
    add_variable(
        x[i, j, k],
        i = 1L:NUM_SPOTS,
        j = 1L:NUM_SPOTS,
        k = 1L:num_workers,
        type = "binary"
    ) %>%

    # helper variable for the MTZ sub-tour constraints
    add_variable(
        u[i, k],
        i = 1L:NUM_SPOTS,
        k = 1L:num_workers,
        lb = 1,
        ub = NUM_SPOTS
    ) %>%

    # minimize travel distance and latest arrival
    set_objective(
        sum_expr(
            distance_matrix[i, j] * x[i, j, k],
            i = 1L:NUM_SPOTS,
            j = 1L:NUM_SPOTS,
            k = 1L:num_workers
        ), "min"
    ) %>%

    # you cannot go to the same city
    add_constraint(
        x[i, i, k] == 0,
        i = same_city_constraint_applies_to,
        k = 1L:num_workers,
        .show_progress_bar = FALSE
    ) %>%

    # each salesman needs to leave the depot
    add_constraint(
        sum_expr(
            x[1L, j, k],
            j = 1L:NUM_SPOTS # j = 2L:NUM_SPOTS
        ) == 1,
        k = 1L:num_workers,
        .show_progress_bar = FALSE
    ) %>%

    # each salesman needs to come back to the depot
    add_constraint(
        sum_expr(
            x[i, 1, k],
            i = 1L:NUM_SPOTS # i = 2L:NUM_SPOTS
        ) == 1,
        k = 1L:num_workers,
        .show_progress_bar = FALSE
    ) %>%

    # if a salesman comes to a city he has to leave it as well
    add_constraint(
        sum_expr(
            x[j, i, k],
            j = 1L:NUM_SPOTS
        ) == sum_expr(
            x[i, j, k],
            j = 1L:NUM_SPOTS
        ),
        i = 2L:NUM_SPOTS,
        k = 1L:num_workers,
        .show_progress_bar = FALSE
    ) %>%

    # leave each city with only one salesman
    add_constraint(
        sum_expr(
            x[i, j, k],
            j = 1L:NUM_SPOTS,
            k = 1L:num_workers
        ) == l_selected[i], # 1,
        i = 2L:NUM_SPOTS,
        .show_progress_bar = FALSE
    ) %>%

    # arrive at each city with only one salesman
    # add_constraint(
    #     sum_expr(
    #         x[i, j, k],
    #         i = 1L:NUM_SPOTS,
    #         k = 1L:num_workers
    #     ) == 1,
    #     j = 2L:NUM_SPOTS
    # ) %>%

    # ensure no subtours (arc constraints)
    add_constraint(
        u[i, k] >= 2,
        i = 2L:NUM_SPOTS,
        k = 1L:num_workers,
        .show_progress_bar = FALSE
    ) %>%

    add_constraint(
        u[i, k] - u[j, k] + 1 <= (NUM_SPOTS - 1) * (1 - x[i, j, k]),
        i = 2L:NUM_SPOTS,
        j = 2L:NUM_SPOTS,
        k = 1L:num_workers,
        .show_progress_bar = FALSE
    )

    # add the workload constraint if max_cost_woker is defined
    if(is.finite(max_cost_worker)) {
        model <- add_constraint(
            .model = model,
            sum_expr(
                x[i, j, k] * (distance_matrix[i, j] + spot_cali_cost[j]),
                i = 1L:NUM_SPOTS,
                j = 1L:NUM_SPOTS
            ) <= max_cost_worker,
            k = 1L:num_workers,
            .show_progress_bar = FALSE
        )
    }

    # result object is made globally available for testing purpose
    ompr_result <<- tryCatch(
        solve_model(model, with_ROI(solver = "gurobi")),
        error = function(e) {
            NULL # RETURN
        }
    )

    # early termination in error
    if(is.null(ompr_result)) {
        return(NULL)
    }

    solution <- get_solution(
        ompr_result, x[i, j, k]
    ) %>% filter(value > 0 & i != j)
    if(nrow(solution) == 0L) { return(NULL) }
    unique_workers <- unique(solution$k)
    actual_num_workers <- length(unique_workers)

    # construct array of paths
    paths_array <- array(
        data = rep(
            0L,
            NUM_SPOTS * NUM_SPOTS * actual_num_workers
        ), dim = c(NUM_SPOTS, NUM_SPOTS, actual_num_workers),
        dimnames = list(
            z_nd_str("spot", NUM_SPOTS),
            z_nd_str("spot", NUM_SPOTS),
            z_nd_str("worker", actual_num_workers)
        )
    )
    worker_id_map = rep(0L, num_workers)
    worker_id_map[unique_workers] = 1L:actual_num_workers
    if(paranoid) {
        stopifnot(
            all(worker_id_map >= 0L) && all(worker_id_map <= actual_num_workers)
        )

        # guarantee each worker id in array is mapped from exactly one worker
        for(worker_id in 1L:actual_num_workers) {
            stopifnot(sum(worker_id_map == worker_id) == 1L)
        }
    }

    # fill in the array
    for(jnd in 1L:nrow(solution)) {
        worker_id <- worker_id_map[solution$k[jnd]]
        paths_array[solution$i[jnd], solution$j[jnd], worker_id] <-
            as.integer(solution$value[jnd] > 0)
    }
    paths_array # RETURN
}

} # ENDIF
