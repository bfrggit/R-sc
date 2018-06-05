# runner.R
#
# Created: 2018-06-04
#  Author: Charles Zhu

run_no_move <<- function(
    sensor_type_period,
    sensor_type_cali_t,
    sensor_presence,
    sensor_ttnc_init,
    num_iters,
    selector
) {
    stopifnot(is.integer(sensor_type_period))
    stopifnot(is.integer(sensor_type_cali_t))
    stopifnot(length(sensor_type_period) == NUM_TYPES)
    stopifnot(length(sensor_type_cali_t) == NUM_TYPES)

    stopifnot(is.integer(sensor_presence) && is.matrix(sensor_presence))
    stopifnot(ncol(sensor_presence) == NUM_TYPES)
    stopifnot(nrow(sensor_presence) == NUM_NODES)
    stopifnot(all(sensor_presence == 0L | sensor_presence == 1L))

    stopifnot(is.numeric(sensor_ttnc_init) && is.matrix(sensor_ttnc_init))
    stopifnot(ncol(sensor_ttnc_init) == NUM_TYPES)
    stopifnot(nrow(sensor_ttnc_init) == NUM_NODES)
    stopifnot(all(sensor_ttnc_init >= 0))

    stopifnot(is.integer(num_iters))
    stopifnot(length(num_iters) == 1L)
    stopifnot(num_iters > 0L)

    # begin of simulation
    ttnc <- sensor_ttnc_init
    cali_cost <<- rep(NaN, num_iters)
    intervals <<- rep(NaN, num_iters)

    for(jnd in 1L:num_iters) {
        selected_sensors <- selector(
            sensor_type_period,
            sensor_type_cali_t,
            ttnc
        )
        stopifnot(is.integer(selected_sensors))
        stopifnot(all(selected_sensors == 1L | selected_sensors == 0L))
        stopifnot(all(selected_sensors == 1L | ttnc > 0))

        cali_cost[jnd] <<- sum(
            apply(
                ifelse(
                    selected_sensors,
                    yes = matrix(
                        rep(sensor_type_cali_t, NUM_NODES),
                        nrow = NUM_NODES,
                        byrow = TRUE
                    ),
                    no = 0L
                ),
                MARGIN = 1L,
                max
            )
        )

        # compute state before next iteration
        ttnc_after <- ifelse(
            selected_sensors,
            yes = matrix(
                rep(sensor_type_period, NUM_NODES),
                nrow = NUM_NODES,
                byrow = TRUE
            ),
            no = ttnc
        )
        intervals[jnd] <<- ttni <- min(ttnc_after)
        ttnc <- ttnc_after - ttni
    }

    NULL # RETURN
}
