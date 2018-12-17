# aux.R
#
# Created: 2018-06-07
# Updated: 2018-12-13
#  Author: Charles Zhu
#
# auxiliary functions for the simulator
#
if(!exists("EX_AUX_R")) {
    EX_AUX_R <<- TRUE

get_node_cali_time <<- function(
    st_cali_t,              # sensor type calibration time/cost
    s_selected,             # selected sensors
    paranoid = TRUE         # enable/disable paranoid checks
) {
    if(paranoid) {
        stopifnot(is.integer(st_cali_t))
        stopifnot(length(st_cali_t) == NUM_TYPES)

        stopifnot(is.integer(s_selected) && is.matrix(s_selected))
        stopifnot(ncol(s_selected) == NUM_TYPES)
        stopifnot(nrow(s_selected) == NUM_NODES)
        stopifnot(all(s_selected == 0L | s_selected == 1L))
    }

    apply(
        s_selected %*% diag(st_cali_t), # per-sensor time, N by K
        MARGIN = 1L,
        FUN = max
    ) # RETURN, per-node time, N by 1
}

get_spot_cali_time <<- function(
    st_cali_t,              # sensor type calibration time/cost
    s_selected,             # selected sensors
    n_location,             # node location matrix, needed for multi-party
    multi_cali = TRUE,      # enable/disable multi-party calibration
    paranoid = TRUE         # enable/disable paranoid checks
) {
    if(paranoid) {
        stopifnot(is.integer(n_location) && is.matrix(n_location))
        stopifnot(ncol(n_location) == NUM_SPOTS_POPULATED)
        stopifnot(nrow(n_location) == NUM_NODES)
        stopifnot(all(n_location == 0L | n_location == 1L))
    }

    apply(
        t(n_location) %*% diag(
            get_node_cali_time(
                st_cali_t   = st_cali_t,
                s_selected  = s_selected,
                paranoid    = paranoid
            ) # per-node time, N by 1
        ), # per-spot-node time, L by N
        MARGIN = 1L,
        FUN = ifelse(multi_cali, yes = max, no = sum)
    ) # RETURN, per-spot calibration time
}

get_cali_time <<- function(
    ...
) {
    sum(get_spot_cali_time(...)) # RETURN
}

get_post_ttnc <<- function(
    st_period,              # sensor type calibration period
    ttnc_before,            # the TTNC matrix before an iteration
    s_selected,             # selected sensor for that iteration
    paranoid = TRUE         # enable/disable paranoid checks
) {
    if(paranoid) {
        stopifnot(is.integer(st_period))
        stopifnot(length(st_period) == NUM_TYPES)

        stopifnot(is.numeric(ttnc_before) && is.matrix(ttnc_before))
        stopifnot(ncol(ttnc_before) == NUM_TYPES)
        stopifnot(nrow(ttnc_before) == NUM_NODES)

        stopifnot(is.integer(s_selected) && is.matrix(s_selected))
        stopifnot(ncol(s_selected) == NUM_TYPES)
        stopifnot(nrow(s_selected) == NUM_NODES)
        stopifnot(all(s_selected == 0L | s_selected == 1L))
    }

    ifelse(
        s_selected,
        yes = matrix(
            rep(st_period, NUM_NODES),
            nrow = NUM_NODES,
            byrow = TRUE
        ),
        no = ttnc_before
    ) # RETURN
}

} # ENDIF
