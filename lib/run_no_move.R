# run_no_move.R
#
# Created: 2018-06-04
# Updated: 2018-06-07
#  Author: Charles Zhu

if(!exists("EX_RUN_NO_MOVE_R")) {
    EX_RUN_NO_MOVE_R <<- TRUE

source("lib/basic.R")
source("lib/aux.R")

run_no_move <<- function(
    st_period,          # sensor type calibration period
    st_cali_t,          # sensor type calibration time/cost
    s_presence,         # sensor presence matrix
    ttnc_init,          # initial TTNC matrix
    num_iters,          # number of iterations as stop condition for simulation
    selector_f,         # selector function
    paranoid = TRUE     # enable/disable paranoid checks
) {
    # this function runs a simplified simulation
    # that does not consider the cost of moving;
    # iteration overhead and calibration cost are returned in raw values
    # that are not yet normalized

    if(paranoid) {
        stopifnot(is.integer(st_period))
        stopifnot(length(st_period) == NUM_TYPES)

        stopifnot(is.integer(st_cali_t))
        stopifnot(length(st_cali_t) == NUM_TYPES)

        stopifnot(is.integer(s_presence) && is.matrix(s_presence))
        stopifnot(ncol(s_presence) == NUM_TYPES)
        stopifnot(nrow(s_presence) == NUM_NODES)
        stopifnot(all(s_presence == 0L | s_presence == 1L))

        stopifnot(is.numeric(ttnc_init) && is.matrix(ttnc_init))
        stopifnot(ncol(ttnc_init) == NUM_TYPES)
        stopifnot(nrow(ttnc_init) == NUM_NODES)
        stopifnot(all(ttnc_init >= 0))

        stopifnot(is.integer(num_iters))
        stopifnot(length(num_iters) == 1L)
        stopifnot(num_iters > 0L)
    }

    # prepare for simulation and initialize stat recorder
    cali_cost <- rep(NaN, num_iters)
    intervals <- rep(NaN, num_iters)
    names(cali_cost) <- names(intervals) <- z_nd_str("iter", num_iters)

    ttnc <- ttnc_init
    for(it in 1L:num_iters) {
        # call the sensor selection solver
        selected_sensors <- selector_f(
            st_period   = st_period,
            st_cali_t   = st_cali_t,
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

        # state transition
        cali_cost[it] <- get_cali_time(
            st_cali_t   = st_cali_t,
            s_selected  = selected_sensors,
            paranoid    = paranoid
        )
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
    res$intervals <- intervals
    res # RETURN
}

} # ENDIF
