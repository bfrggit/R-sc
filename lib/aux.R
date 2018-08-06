# aux.R
#
# Created: 2018-06-07
# Updated: 2018-08-05
#  Author: Charles Zhu
#
# auxiliary functions for the simulator
#
if(!exists("EX_AUX_R")) {
    EX_AUX_R <<- TRUE

# the following version does not consider, multi-party calibration, i.e.
# multiple nodes at the same spot can have their sensors calibrated at once
# use the newer version IF assuming multi-party calibration
# get_cali_time <<- function(
#     st_cali_t,              # sensor type calibration time/cost
#     s_selected,             # selected sensors
#     paranoid = TRUE         # enable/disable paranoid checks
# ) {
#     if(paranoid) {
#         stopifnot(is.integer(st_cali_t))
#         stopifnot(length(st_cali_t) == NUM_TYPES)
#
#         stopifnot(is.integer(s_selected) && is.matrix(s_selected))
#         stopifnot(ncol(s_selected) == NUM_TYPES)
#         stopifnot(nrow(s_selected) == NUM_NODES)
#         stopifnot(all(s_selected == 0L | s_selected == 1L))
#     }
#
#     sum(
#         apply(
#             s_selected %*% diag(st_cali_t),
#             MARGIN = 1L,
#             max
#         )
#     ) # RETURN
# }

get_cali_time <<- function(
    st_cali_t,              # sensor type calibration time/cost
    s_selected,             # selected sensors
    n_location,             # node location matrix, needed for multi-party
    paranoid = TRUE         # enable/disable paranoid checks
) {
    if(paranoid) {
        stopifnot(is.integer(st_cali_t))
        stopifnot(length(st_cali_t) == NUM_TYPES)

        stopifnot(is.integer(s_selected) && is.matrix(s_selected))
        stopifnot(ncol(s_selected) == NUM_TYPES)
        stopifnot(nrow(s_selected) == NUM_NODES)
        stopifnot(all(s_selected == 0L | s_selected == 1L))

        stopifnot(is.integer(n_location) && is.matrix(n_location))
        stopifnot(ncol(n_location) == NUM_SPOTS_POPULATED)
        stopifnot(nrow(n_location) == NUM_NODES)
        stopifnot(all(n_location == 0L | n_location == 1L))
    }

    sum(
        apply(
            t(n_location) %*% diag(
                apply(
                    s_selected %*% diag(st_cali_t), # per-sensor time, N by K
                    MARGIN = 1L,
                    FUN = max
                ) # per-node time, N by 1
            ), # per-spot-node time, L by N
            MARGIN = 1L,
            FUN = max
        ) # per-spot calibration time
    ) # RETURN
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
