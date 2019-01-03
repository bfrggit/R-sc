# naive_sel_lim.R
#
# Created: 2018-12-13
# Updated: 2019-01-02
#  Author: Charles Zhu
#
if(!exists("EX_NAIVE_SEL_LIM_R")) {
    EX_NAIVE_SEL_LIM_R <<- TRUE

source("lib/naive_sel.R")

lim_get_mask <- function(st_cali_t, node_cali_t) {
    matrix(
        rep(st_cali_t, NUM_NODES),
        nrow = NUM_NODES,
        byrow = TRUE
    ) <= matrix( # type cali_t not greater than node cali_t
        rep(node_cali_t, NUM_TYPES),
        ncol = NUM_TYPES,
        byrow = FALSE
    ) # RETURN, mask matrix, N by K, if TRUE, this sensor's cali_t fits
}

lim_add_nodal <- function(st_cali_t, s_presence,
    s_selected, # selected sensors
    paranoid
) {             # add sensors on selected nodes with smaller cali_t
    if(paranoid) {
        stopifnot(is.integer(st_cali_t))
        stopifnot(length(st_cali_t) == NUM_TYPES)

        stopifnot(is.integer(s_presence) && is.matrix(s_presence))
        stopifnot(ncol(s_presence) == NUM_TYPES)
        stopifnot(nrow(s_presence) == NUM_NODES)
        stopifnot(all(s_presence == 0L | s_presence == 1L))

        stopifnot(is.integer(s_selected) && is.matrix(s_selected))
        stopifnot(nrow(s_selected) == NUM_NODES)
        stopifnot(ncol(s_selected) == NUM_TYPES)
        stopifnot(all(s_selected == 0L | s_selected == 1L))
        stopifnot(all(s_selected <= s_presence))
    }

    node_cali_t <- apply(
        s_selected %*% diag(st_cali_t), # per-sensor time, N by K
        MARGIN = 1L,
        FUN = max
    ) # per-node time, length N

    lim_mask <- lim_get_mask(st_cali_t, node_cali_t)
    s_selected_nodal <- naive_add_nodal(s_presence, s_selected)
    res <- s_selected_nodal * lim_mask

    if(paranoid) {
        stopifnot(is.matrix(res))
        stopifnot(nrow(res) == NUM_NODES)
        stopifnot(ncol(res) == NUM_TYPES)
        stopifnot(all(res <= s_presence))
        stopifnot(all(res >= s_selected))
    }

    res # RETURN
}

get_sel_f_nodal_lim <- function(st_cali_t, s_presence, ...) {
    stopifnot(is.integer(s_presence) && is.matrix(s_presence))
    stopifnot(ncol(s_presence) == NUM_TYPES)
    stopifnot(nrow(s_presence) == NUM_NODES)
    stopifnot(all(s_presence == 0L | s_presence == 1L))

    function(
        ttnc_before,
        paranoid = TRUE
    ) {
        lim_add_nodal(
            st_cali_t       = st_cali_t,
            s_presence      = s_presence,
            s_selected      = sel_f_minimal(
                ttnc_before = ttnc_before,
                paranoid    = paranoid
            ), # minimal selection matrix, N by K
            paranoid        = paranoid
        ) # RETURN
    } # RETURN
}

lim_add_local <- function(st_cali_t, n_location, s_presence,
    s_selected, # selected sensors
    multi_cali, # if not enabled, degrade to lim_add_nodal
    paranoid
) {
    if(paranoid) {
        stopifnot(is.integer(s_presence) && is.matrix(s_presence))
        stopifnot(ncol(s_presence) == NUM_TYPES)
        stopifnot(nrow(s_presence) == NUM_NODES)
        stopifnot(all(s_presence == 0L | s_presence == 1L))
    }

    stopifnot(multi_cali) # if multi-party is disabled, use lim_add_nodal

    spot_cali_t <- get_spot_cali_time(
        st_cali_t   = st_cali_t,
        s_selected  = s_selected,
        n_location  = n_location,
        multi_cali  = multi_cali,
        paranoid    = paranoid
    ) # per-spot time, length L

    node_cali_t <- n_location %*% spot_cali_t # per-node time, length N
    lim_mask <- lim_get_mask(st_cali_t, node_cali_t)
    s_selected_local <- naive_add_local(n_location, s_presence, s_selected)
    res <- s_selected_local * lim_mask

    if(paranoid) {
        stopifnot(is.matrix(res))
        stopifnot(nrow(res) == NUM_NODES)
        stopifnot(ncol(res) == NUM_TYPES)
        stopifnot(all(res <= s_presence))
        stopifnot(all(res >= s_selected))
    }

    res # RETURN
}

get_sel_f_local_lim <- function(st_cali_t, n_location, s_presence,
    multi_cali, ...
) {
    stopifnot(is.integer(n_location) && is.matrix(n_location))
    stopifnot(ncol(n_location) == NUM_SPOTS_POPULATED)
    stopifnot(nrow(n_location) == NUM_NODES)
    stopifnot(all(n_location == 0L | n_location == 1L))

    stopifnot(is.integer(s_presence) && is.matrix(s_presence))
    stopifnot(ncol(s_presence) == NUM_TYPES)
    stopifnot(nrow(s_presence) == NUM_NODES)
    stopifnot(all(s_presence == 0L | s_presence == 1L))

    # if multi-party calibration is disabled,
    #   this method should degrade to sel_f_nodal_lim
    if(!multi_cali) {
        return(
            get_sel_f_nodal_lim(st_cali_t, s_presence)
        )
    }

    function(
        ttnc_before,
        paranoid = TRUE
    ) {
        lim_add_local(
            st_cali_t       = st_cali_t,
            n_location      = n_location,
            s_presence      = s_presence,
            s_selected      = sel_f_minimal(
                ttnc_before = ttnc_before,
                paranoid    = paranoid
            ), # minimal selection matrix, N by K
            multi_cali      = multi_cali,
            paranoid        = paranoid
        ) # RETURN
    } # RETURN
}

} # ENDIF
