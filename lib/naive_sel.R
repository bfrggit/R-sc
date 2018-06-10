# naive_sel.R
#
# Created: 2018-06-09
# Updated: 2018-06-10
#  Author: Charles Zhu
#
if(!exists("EX_NAIVE_SEL_R")) {
    EX_NAIVE_SEL_R <<- TRUE

sel_f_minimal <- function(
    ...,
    ttnc_before,
    paranoid = TRUE
) {
    if(paranoid) {
        stopifnot(is.numeric(ttnc_before) && is.matrix(ttnc_before))
        stopifnot(ncol(ttnc_before) == NUM_TYPES)
        stopifnot(nrow(ttnc_before) == NUM_NODES)
        stopifnot(all(ttnc_before >= 0))
    }

    ttnc_min = min(ttnc_before)
    ifelse(
        ttnc_before == ttnc_min,
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
        ...,
        ttnc_before,
        paranoid = TRUE
    ) {
        # paranoid check is done by sel_minimal which is called immediately here
        res = sel_f_minimal(
            ...,
            ttnc_before = ttnc_before,
            paranoid = paranoid
        )
        nodal_selection = apply(
            res,
            MARGIN = 1L,
            FUN = function(v) {
                any(v == 1L)
            }
        )

        for(jnd in 1L:nrow(res)) {
            if(nodal_selection[jnd]) {
                res[jnd, ] <- s_presence[jnd, ]
            }
        }
        res # RETURN
    } # RETURN
}

} # ENDIF
