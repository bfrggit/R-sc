# run.R
#
# Created: 2018-10-05
# Updated: 2018-12-17
#  Author: Charles Zhu
#
# derived from run_no_move.R
#
if(!exists("EX_RUN_R")) {
    EX_RUN_R <<- TRUE

source("lib/basic.R")
source("lib/aux.R")
source("lib/graph_aux.R")

run <<- function(
    st_period,          # sensor type calibration period
    st_cali_t,          # sensor type calibration time/cost
    n_location,         # node location matrix
    s_presence,         # sensor presence matrix
    distance_matrix,    # distance matrix calculated from the map graph
    ttnc_init,          # initial TTNC matrix
    num_iters,          # number of iterations as stop condition for simulation
    sum_intervals,      # total time period as stop condition for simulation
    selector_f,         # selector function
    path_plan_f,        # path planner function
    multi_cali = TRUE,      # enable/disable multi-party calibration
    max_cost_worker = +Inf, # constraint
    paranoid = TRUE,        # enable/disable paranoid checks
    pp_paranoid = FALSE,    # enable/disable paranoid for path planner
    verbose = FALSE,        # enable/disable verbose output for debugging
    keep_history = FALSE    # enable/disable result history keeping
) {
    # in this simulation,
    # metrics are returned in raw values that are not yet normalized

    if(TRUE) { # if(paranoid) {
        stopifnot(is.integer(st_period))
        stopifnot(length(st_period) == NUM_TYPES)

        stopifnot(is.integer(st_cali_t))
        stopifnot(length(st_cali_t) == NUM_TYPES)

        stopifnot(is.integer(n_location) && is.matrix(n_location))
        stopifnot(ncol(n_location) == NUM_SPOTS_POPULATED)
        stopifnot(nrow(n_location) == NUM_NODES)
        stopifnot(all(n_location == 0L | n_location == 1L))
        stopifnot(all(n_location[, 1L]) == 0L) # no node at depot
        stopifnot(all(rowSums(n_location) == 1))

        stopifnot(is.integer(s_presence) && is.matrix(s_presence))
        stopifnot(ncol(s_presence) == NUM_TYPES)
        stopifnot(nrow(s_presence) == NUM_NODES)
        stopifnot(all(s_presence == 0L | s_presence == 1L))

        stopifnot(is.matrix(distance_matrix))
        stopifnot(is.integer(distance_matrix))
        stopifnot(ncol(distance_matrix) == NUM_SPOTS)
        stopifnot(nrow(distance_matrix) == NUM_SPOTS)
        stopifnot(all(distance_matrix) >= 0L)

        stopifnot(is.numeric(ttnc_init) && is.matrix(ttnc_init))
        stopifnot(ncol(ttnc_init) == NUM_TYPES)
        stopifnot(nrow(ttnc_init) == NUM_NODES)
        stopifnot(all(ttnc_init >= 0))

        stopifnot(is.null(num_iters) || is.null(sum_intervals))
        if(is.null(num_iters)) {
            stopifnot(is.integer(sum_intervals))
            stopifnot(length(sum_intervals) == 1L)
            stopifnot(sum_intervals > 0L)
        } else {
            stopifnot(is.integer(num_iters))
            stopifnot(length(num_iters) == 1L)
            stopifnot(num_iters > 0L)
        }

        stopifnot(is.function(selector_f))
        stopifnot(is.function(path_plan_f) || is.null(path_plan_f))

        stopifnot(is.numeric(max_cost_worker))
        stopifnot(length(max_cost_worker) == 1L)
        stopifnot(max_cost_worker > 0)
    }

    if(verbose) {
        cat("---\n")
        cat("Simulation started.\n")
    }

    # prepare for simulation and initialize stat recorder
    cali_cost <- numeric()
    move_cost <- numeric()
    intervals <- numeric()
    num_paths <- numeric()
    selc_time <- numeric()
    path_time <- numeric()

    # result history keeping
    history_selection <- NULL
    history_paths <- NULL
    if(keep_history) {
        history_selection <- list()
        history_paths <- list()
    }

    ttnc <- ttnc_init - min(ttnc_init)
    it <- 0L
    acc_intervals <- 0
    while(is.integer(num_iters) &&
        it < num_iters ||
        is.integer(sum_intervals) &&
        acc_intervals < sum_intervals
    ) {
        # count iterations
        it <- it + 1L

        if(verbose) {
            cat(sprintf("it = %d,", it), "")
        }

        # call the sensor selection solver
        selc_time[it] <- system.time(
            selected_sensors <- selector_f(
                ttnc_before = ttnc
            )
        )[3]

        if(paranoid) {
            stopifnot(is.integer(selected_sensors)
                    && is.matrix(selected_sensors))
            stopifnot(ncol(selected_sensors) == NUM_TYPES)
            stopifnot(nrow(selected_sensors) == NUM_NODES)
            stopifnot(all(selected_sensors == 1L | selected_sensors == 0L))
            stopifnot(all(selected_sensors <= s_presence))
            stopifnot(all(selected_sensors == 1L | ttnc > 0))
        }

        # find selected spots
        selected_spots <- get_selected_spots_from_selected_sensors(
            s_selected  = selected_sensors,
            n_location  = n_location,
            paranoid    = paranoid
        )

        if(verbose) {
            cat(
                sprintf("sel_sensors = %d,", sum(selected_sensors)),
                sprintf("sel_spots = %d", sum(selected_spots)),
                "->", ""
            )
        }

        # calibration cost
        spot_cali_cost <- get_spot_cali_time(
            st_cali_t   = st_cali_t,
            s_selected  = selected_sensors,
            n_location  = n_location,
            multi_cali  = multi_cali,
            paranoid    = paranoid
        )
        cali_cost[it] <- sum(spot_cali_cost)

        if(paranoid) {
            stopifnot(spot_cali_cost[1L] == 0)
        }

        if(is.null(path_plan_f)) {
            path_time[it] <- NA
            move_cost[it] <- NA
            num_paths[it] <- NA
        } else {
            # preprocess for calculating movement cost
            path_time[it] = system.time(
                paths_array <- path_plan_f(
                    l_selected      = selected_spots,
                    distance_matrix = distance_matrix,
                    max_cost_worker = max_cost_worker,
                    spot_cali_cost  = spot_cali_cost,
                    paranoid        = pp_paranoid # paranoid
                )
            )[3]

            if(paranoid) {
                stopifnot(
                    is_valid_multi_paths_array(
                        paths_array = paths_array,
                        l_selected = selected_spots,
                        must_start_from_spot = 1L,
                        paranoid = paranoid
                    )
                )
            }

            # movement cost
            move_cost_per_worker <- get_move_dist_per_worker(
                distance_matrix = distance_matrix,
                paths_array     = paths_array,
                paranoid        = paranoid
            )
            move_cost[it] <- sum(move_cost_per_worker)
            num_paths[it] <- sum(move_cost_per_worker > 0)
        }

        if(verbose) {
            cat(
                sprintf("move_cost = %d,", move_cost[it]),
                sprintf("num_paths = %d\n", num_paths[it])
            )
        }

        if(keep_history) {
            history_selection[[length(history_selection) + 1L]] <-
                selected_spots
            history_paths[[length(history_paths) + 1L]] <-
                paths_array
        }

        # state transition
        ttnc_after <- get_post_ttnc(
            st_period   = st_period,
            ttnc_before = ttnc,
            s_selected  = selected_sensors,
            paranoid    = paranoid
        )
        intervals[it] <- ttni <- min(ttnc_after)
        ttnc <- ttnc_after - ttni
        acc_intervals <- acc_intervals + ttni
    }

    if(verbose) {
        cat("Simulation complete.\n")
        cat("---\n")
    }

    names(cali_cost) <-
        names(move_cost) <-
        names(intervals) <-
        names(num_paths) <-
        names(selc_time) <-
        names(path_time) <-
        z_nd_str("iter", it)

    if(keep_history) {
        names(history_selection) <-
            names(history_paths) <-
            z_nd_str("iter", it)
    }

    res <- list()
    res$num_iters <- it
    res$sum_intervals <- acc_intervals
    res$cali_cost <- cali_cost
    res$move_cost <- move_cost
    res$intervals <- intervals
    res$num_paths <- num_paths
    res$selc_time <- selc_time
    res$path_time <- path_time
    res$history_selection <- history_selection
    res$history_paths <- history_paths
    res # RETURN
}

} # ENDIF
