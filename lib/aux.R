# aux.R
#
# Created: 2018-06-07
#  Author: Charles Zhu
#
# auxiliary functions for the simulator
#
if(!exists("EX_AUX_R")) {
    EX_AUX_R <<- TRUE

get_cali_time <<- function(
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
        stopifnot(all(s_selected == 0 | s_selected == 1))
    }

    sum(
        apply(
            s_selected %*% diag(st_cali_t),
            MARGIN = 1,
            max
        )
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
        stopifnot(all(s_selected == 0 | s_selected == 1))
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
