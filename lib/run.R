# run.R
#
# Created: 2018-10-05
# Updated: 2018-10-08
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
    selector_f,         # selector function
    path_plan_f,        # path planner function
    paranoid = TRUE     # enable/disable paranoid checks
) {
    # in this simulation,
    # metrics are returned in raw values that are not yet normalized

    if(paranoid) {
        stopifnot(is.integer(st_period))
        stopifnot(length(st_period) == NUM_TYPES)

        stopifnot(is.integer(st_cali_t))
        stopifnot(length(st_cali_t) == NUM_TYPES)

        stopifnot(is.integer(n_location) && is.matrix(n_location))
        stopifnot(ncol(n_location) == NUM_SPOTS_POPULATED)
        stopifnot(nrow(n_location) == NUM_NODES)
        stopifnot(all(n_location == 0L | n_location == 1L))

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

        stopifnot(is.integer(num_iters))
        stopifnot(length(num_iters) == 1L)
        stopifnot(num_iters > 0L)

        stopifnot(is.function(selector_f))
        stopifnot(is.function(path_plan_f))
    }

    # prepare for simulation and initialize stat recorder
    cali_cost <- rep(NaN, num_iters)
    move_cost <- rep(NaN, num_iters)
    intervals <- rep(NaN, num_iters)
    num_paths <- rep(NaN, num_iters)
    names(cali_cost) <-
        names(move_cost) <-
        names(intervals) <-
        names(num_paths) <-
        z_nd_str("iter", num_iters)

    ttnc <- ttnc_init - min(ttnc_init)
    for(it in 1L:num_iters) {
        # call the sensor selection solver
        selected_sensors <- selector_f(
            ttnc_before = ttnc
        )

        if(paranoid) {
            stopifnot(is.integer(selected_sensors)
                    && is.matrix(selected_sensors))
            stopifnot(ncol(selected_sensors) == NUM_TYPES)
            stopifnot(nrow(selected_sensors) == NUM_NODES)
            stopifnot(all(selected_sensors == 1L | selected_sensors == 0L))
            stopifnot(all(selected_sensors == 1L | ttnc > 0))
        }

        # calibration cost
        cali_cost[it] <- get_cali_time(
            st_cali_t   = st_cali_t,
            s_selected  = selected_sensors,
            n_location  = n_location,
            paranoid    = paranoid
        )

        # preprocess for calculating movement cost
        selected_spots <- get_selected_spots_from_selected_sensors(
            s_selected  = selected_sensors,
            n_location  = n_location,
            paranoid    = paranoid
        )
        paths_array <- path_plan_f(
            l_selected  = selected_spots,
            paranoid    = paranoid
        )

        if(paranoid) {
            stopifnot(
                is_valid_multi_paths_array(
                    paths_array = paths_array,
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
        # move_cost[it] <- get_move_dist(
        #     distance_matrix = distance_matrix,
        #     paths_array     = paths_array,
        #     paranoid        = paranoid
        # )
        move_cost[it] <- sum(move_cost_per_worker)
        num_paths[it] <- sum(move_cost_per_worker > 0)

        # state transition
        ttnc_after <- get_post_ttnc(
            st_period   = st_period,
            ttnc_before = ttnc,
            s_selected  = selected_sensors,
            paranoid    = paranoid
        )
        intervals[it] <- ttni <- min(ttnc_after)
        ttnc <- ttnc_after - ttni
    }

    res <- list()
    res$cali_cost <- cali_cost
    res$move_cost <- move_cost
    res$intervals <- intervals
    res$num_paths <- num_paths
    res # RETURN
}

} # ENDIF
