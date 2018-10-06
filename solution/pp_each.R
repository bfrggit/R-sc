# pp_each.R
#
# Created: 2018-10-04
#  Author: Charles Zhu
#
# mTSP (path planning) solver
# naive solution
# each spot is visited by a distinct worker

if(!exists("EX_PP_EACH_R")) {
    EX_PP_EACH_R <<- TRUE

source("lib/basic.R")

get_multi_paths_each <<- function(
    l_selected,     # selected spots, len L vector
    paranoid = TRUE # enable/disable paranoid checks
) {
    if(paranoid) {
        stopifnot(is.vector(l_selected))
        stopifnot(is.integer(l_selected))
        stopifnot(length(l_selected) == NUM_SPOTS)
        stopifnot(all(l_selected == 0L | l_selected == 1L))
        stopifnot(l_selected[1] == 0L)
    }

    num_workers <- length(which(l_selected > 0))
    paths_array <- array(
        data = rep(
            0L,
            NUM_SPOTS * NUM_SPOTS * num_workers
        ), dim = c(NUM_SPOTS, NUM_SPOTS, num_workers),
        dimnames = list(
            z_nd_str("spot", NUM_SPOTS),
            z_nd_str("spot", NUM_SPOTS),
            z_nd_str("worker", num_workers)
        )
    )

    worker <- 1L
    for(spot in which(l_selected > 0)) {
        paths_array[1L, spot, worker] = paths_array[spot, 1L, worker] <- 1L
        worker <- worker + 1L
    }
    paths_array # RETURN
}

} # ENDIF
