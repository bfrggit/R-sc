# sel_score_1.R
#
# Created: 2018-12-16
#  Author: Charles Zhu
#
# sensor selection planner
# greedy implmentation driven by interval length

if(!exists("EX_SEL_INTERVAL_1_R")) {
    EX_SEL_INTERVAL_1_R <<- TRUE

source("lib/aux.R")
source("lib/naive_sel_lim.R")
source("solution/pp_greedy_1.R")

get_sel_f_interval_1 <- function(
    n_location,         # node location matrix
    s_presence,         # sensor presence matrix
    distance_matrix,    # distance matrix calculated from the map graph
    st_period,          # sensor type calibration period
    st_cali_t,          # sensor type calibration time/cost
    multi_cali,         # need this to compute cali time
    max_cost_worker,    # need this to estimate move time
    weight_overhead,
    weight_cali,
    weight_move,
    verbose = FALSE     # enable/disable verbose output for debugging
) {
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
    stopifnot(ncol(distance_matrix) == NUM_SPOTS_POPULATED) # NUM_SPOTS
    stopifnot(nrow(distance_matrix) == NUM_SPOTS_POPULATED) # NUM_SPOTS
    stopifnot(all(distance_matrix) >= 0L)

    stopifnot(is.integer(st_period))
    stopifnot(length(st_period) == NUM_TYPES)

    stopifnot(is.integer(st_cali_t))
    stopifnot(length(st_cali_t) == NUM_TYPES)

    function(
        ttnc_before,
        paranoid = TRUE
    ) {
        if(verbose) {
            cat("-> Selection algorithm: interval_1\n")
        }

        # find all possible values of TTNI, i.e. time to next iteration
        st_pr_min <- min(st_period)
        cand_ttni <- sort(unique(c(ttnc_before[
                which(ttnc_before <= st_pr_min & ttnc_before > 0)
            ], st_pr_min)))

        if(verbose) {
            cat("TTNI candidates:", paste(cand_ttni, collapse = ", "), "\n")
        }

        # history records
        history_sel <- list()
        history_effi <- numeric()

        # loop through all TTNI candidates
        # for each possible TTNI, select all sensors with TTNC smaller than it
        for(test_ttni in cand_ttni) {
            if(test_ttni <= 0) { next }

            sel <- ifelse(
                ttnc_before < test_ttni,
                yes = 1L,
                no = 0L
            )

            if(paranoid) {
                stopifnot(is.matrix(sel))
                stopifnot(is.integer(sel))
                stopifnot(nrow(sel) == nrow(s_presence))
                stopifnot(ncol(sel) == ncol(s_presence))
                stopifnot(all(sel == 0L | sel == 1L))
                stopifnot(all(sel <= s_presence))
            }

            # also select all sensors at the same spot or on the same node
            # whichever do not affect local calibration time
            if(multi_cali){
                sel <- lim_add_local(
                    st_cali_t   = st_cali_t,
                    n_location  = n_location,
                    s_presence  = s_presence,
                    s_selected  = sel,
                    multi_cali  = multi_cali,
                    paranoid    = paranoid
                )
            } else {
                sel <- lim_add_nodal(
                    st_cali_t   = st_cali_t,
                    s_presence  = s_presence,
                    s_selected  = sel,
                    paranoid    = paranoid
                )
            }

            # find selected spots
            l_selected <- get_selected_spots_from_selected_sensors(
                s_selected  = sel,
                n_location  = n_location,
                paranoid    = paranoid
            )

            # compute time-to-next-iteration based on init plan
            ttnc_after <- get_post_ttnc(
                st_period   = st_period,
                ttnc_before = ttnc_before,
                s_selected  = sel,
                paranoid    = paranoid
            )
            ttni <- min(ttnc_after)

            if(paranoid) {
                stopifnot(ttni >= test_ttni)
            }

            # compute calibration cost of init plan
            spot_cali_cost <- NULL
            if(multi_cali) {
                spot_cali_cost <- get_spot_cali_time(
                    st_cali_t   = st_cali_t,
                    s_selected  = sel,
                    n_location  = n_location,
                    multi_cali  = multi_cali,
                    paranoid    = paranoid
                )
            } else {
                node_cali_cost <- get_node_cali_time(
                    st_cali_t   = st_cali_t,
                    s_selected  = sel,
                    paranoid    = paranoid
                )
                spot_cali_cost <-
                    rowSums(t(n_location) %*% diag(node_cali_cost))
            }
            cali_cost <- sum(spot_cali_cost)

            # compute movement cost of init plan
            greedy_tour_list <- get_multi_paths_greedy_1(
                l_selected      = l_selected,
                distance_matrix = distance_matrix,
                max_cost_worker = max_cost_worker,
                spot_cali_cost  = spot_cali_cost,
                raw_return      = TRUE,
                paranoid        = paranoid
            )
            greedy_multi_cali_sum <- greedy_list_get_multi_cali_sum(
                tour_list       = greedy_tour_list,
                l_selected      = l_selected,
                spot_cali_cost  = spot_cali_cost,
                paranoid        = paranoid
            )
            greedy_multi_tour_len <- greedy_list_get_multi_tour_len(
                tour_list       = greedy_tour_list,
                l_selected      = l_selected,
                distance_matrix = distance_matrix,
                start_from_spot = 1L,
                paranoid        = paranoid
            )
            move_cost <- sum(greedy_multi_tour_len)
            greedy_cost_sum <- greedy_multi_tour_len + greedy_multi_cali_sum

            # compute efficiency of current selection
            plan_effi <- (weight_overhead +
                weight_cali * cali_cost +
                weight_move * move_cost) / ttni

            history_sel[[length(history_sel) + 1L]] <- sel
            history_effi[length(history_effi) + 1L] <- plan_effi

            if(verbose){
                cat(sprintf("%-43s", paste(
                    sprintf("%8s,", sprintf("(%d)", length(history_sel))),
                    sprintf("ttni = %d,", ttni),
                    sprintf("sel = %d at %d,", sum(sel), sum(l_selected))
                )),
                sprintf("effi = %.3f\n", plan_effi)
                )
            }
        }

        # find the best plan
        best_selection_id <- which(
            history_effi == min(history_effi)
        )[1L]

        if(verbose) {
            cat("Selection algorithm done.\n")
            cat(sprintf("%-43s", paste(
                sprintf("%8s,", sprintf("(%d)", best_selection_id)),
                sprintf("ttni = %d,", ttni)
            )),
            sprintf("effi = %.3f", history_effi[best_selection_id]),
            "\n<- ")
        }

        history_sel[[best_selection_id]] # RETURN
    } # RETURN
}

}
