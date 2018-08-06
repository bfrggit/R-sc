# naive_sel.R
#
# Created: 2018-06-09
# Updated: 2018-08-05
#  Author: Charles Zhu
#
if(!exists("EX_NAIVE_SEL_R")) {
    EX_NAIVE_SEL_R <<- TRUE

sel_f_minimal <- function(
    ttnc_before,
    paranoid = TRUE
) {
    if(paranoid) {
        stopifnot(is.numeric(ttnc_before) && is.matrix(ttnc_before))
        stopifnot(ncol(ttnc_before) == NUM_TYPES)
        stopifnot(nrow(ttnc_before) == NUM_NODES)
        stopifnot(all(ttnc_before >= 0))
    }

    ifelse(
        ttnc_before <= 0,
        yes = 1L,
        no = 0L
    ) # RETURN
}

get_sel_f_all <- function(s_presence) {
    stopifnot(is.integer(s_presence) && is.matrix(s_presence))
    stopifnot(ncol(s_presence) == NUM_TYPES)
    stopifnot(nrow(s_presence) == NUM_NODES)
    stopifnot(all(s_presence == 0L | s_presence == 1L))

    function(
        ...,
        paranoid = TRUE
    ) {
        s_presence # RETURN
    } # RETURN
}

get_sel_f_nodal <- function(s_presence) {
    stopifnot(is.integer(s_presence) && is.matrix(s_presence))
    stopifnot(ncol(s_presence) == NUM_TYPES)
    stopifnot(nrow(s_presence) == NUM_NODES)
    stopifnot(all(s_presence == 0L | s_presence == 1L))

    function(
        ttnc_before,
        paranoid = TRUE
    ) {
        apply(
            diag(
                apply(
                    sel_f_minimal(
                        ttnc_before = ttnc_before,
                        paranoid = paranoid
                    ), # minimal selection matrix, N by K
                    MARGIN = 1L,
                    FUN = max
                ) # nodal selection, N by 1
            ) %*% s_presence,
            MARGIN = c(1L, 2L),
            FUN = as.integer
        ) # RETURN
    } # RETURN
}

get_sel_f_local <- function(n_location, s_presence) {
    stopifnot(is.integer(n_location) && is.matrix(n_location))
    stopifnot(ncol(n_location) == NUM_SPOTS_POPULATED)
    stopifnot(nrow(n_location) == NUM_NODES)
    stopifnot(all(n_location == 0L | n_location == 1L))

    function(
        ttnc_before,
        paranoid = TRUE
    ) {
        apply(
            diag(
                as.integer(
                    n_location %*% matrix(apply(
                        t(n_location) %*% diag( # L by N
                            apply(
                                sel_f_minimal(
                                    ttnc_before = ttnc_before,
                                    paranoid = paranoid
                                ), # minimal selection matrix, N by K
                                MARGIN = 1L,
                                FUN = max
                            ) # nodal selection, N by 1
                        ), # per-spot-node selection, L by N
                        MARGIN = 1L,
                        FUN = max
                    ), ncol = 1L) # nodal selection, N by 1
                )
            ) %*% s_presence,
            MARGIN = c(1L, 2L),
            FUN = as.integer
        ) # RETURN
    } # RETURN
}

} # ENDIF
