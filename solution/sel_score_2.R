# sel_score_2.R
#
# Created: 2018-12-12
# Updated: 2018-12-16
#  Author: Charles Zhu
#
# sensor selection planner
# greedy implmentation with local selection add-on

if(!exists("EX_SEL_SCORE_2_R")) {
    EX_SEL_SCORE_2_R <<- TRUE

source("lib/aux.R")
source("lib/naive_sel.R")
source("lib/naive_sel_lim.R")
source("solution/pp_greedy_1.R")

get_sel_f_score_2 <- function(
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

    sel_f_local_lim_nested <- get_sel_f_local_lim(
        st_cali_t   = st_cali_t,
        n_location  = n_location,
        s_presence  = s_presence,
        multi_cali  = multi_cali
    )

    function(
        ttnc_before,
        paranoid = TRUE
    ) {
        if(verbose) {
            cat("-> Selection algorithm: score_1\n")
        }

        # rolling selection of sensors starting with the minimal selection
        sel <- sel_local_lim <- sel_f_local_lim_nested(
            ttnc_before = ttnc_before,
            paranoid = paranoid
        )

        if(paranoid) {
            stopifnot(is.matrix(sel))
            stopifnot(is.integer(sel))
            stopifnot(nrow(sel) == nrow(s_presence))
            stopifnot(ncol(sel) == ncol(s_presence))
            stopifnot(all(sel == 0L | sel == 1L))
            stopifnot(all(sel <= s_presence))
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
            spot_cali_cost <- rowSums(t(n_location) %*% diag(node_cali_cost))
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

        # history records
        history_sel <- list()
        history_effi <- numeric()
        history_sel[[1L]] <- sel
        history_effi[1L] <- plan_effi

        if(verbose){
            cat(sprintf("%-35s", paste(
                sprintf("%8s,", sprintf("(%d)", length(history_sel))),
                sprintf("sel = %d at %d,", sum(sel), sum(l_selected))
            )),
            sprintf("effi = %.3f\n", plan_effi)
            )
        }

        # list out all unselected node-type pairs (i.e. sensors)
        unsel <- (s_presence - sel > 0)
        pairs_unsel <- which(unsel, arr.ind = TRUE)
        colnames(pairs_unsel) <- c("node", "type")
        cnt_unsel <- nrow(pairs_unsel)

        # loop until all sensors are selected
        while(cnt_unsel > 0) {
            # spot_score_move <- rep(NA, length(l_selected))
            pair_score_eval <- # evaluation of score of each node-type pair
                pair_score_cali <- # individual term in the score eval
              # pair_score_move <-
                pair_score_ttni <-
                ifelse(unsel, yes = NA, no = +Inf)
            spot_score_move <- ifelse(
                l_selected > 0,
                yes = move_cost,
                no = NA
            )

            # loop to test each possible pair
            # pick the one with highest score
            for(test_pair_id in 1L:cnt_unsel) {
                tp <- pairs_unsel[test_pair_id, ] # test pair
                node_tp <- tp[1L]
                type_tp <- tp[2L]
                spot_tp <- which(n_location[node_tp, ] > 0)

                # create temporary selection mat
                sel_tmp <- sel # build temporary selection mat
                sel_tmp[node_tp, type_tp] <- 1L # add test pair
                # l_selected_tmp_old <- get_selected_spots_from_selected_sensors(
                #     s_selected  = sel_tmp,
                #     n_location  = n_location,
                #     paranoid    = paranoid
                # )
                l_selected_tmp <- l_selected
                l_selected_tmp[spot_tp] <- 1L

                # simplification check
                # stopifnot(all(l_selected_tmp == l_selected_tmp_old))

                # compute new interval length
                # ttnc_after_tmp_old <- get_post_ttnc(
                #     st_period   = st_period,
                #     ttnc_before = ttnc_before,
                #     s_selected  = sel_tmp,
                #     paranoid    = paranoid
                # )
                ttnc_after_tmp <- ttnc_after
                ttnc_after_tmp[node_tp, type_tp] <- st_period[type_tp]
                pair_score_ttni[node_tp, type_tp] <- min(ttnc_after_tmp)

                # simplification check
                # stopifnot(all(ttnc_after_tmp == ttnc_after_tmp_old))

                # compute new calibration cost
                if(multi_cali) {
                    pair_score_cali[node_tp, type_tp] <- cali_cost + max(
                        0, st_cali_t[type_tp] - spot_cali_cost[spot_tp])
                } else {
                    pair_score_cali[node_tp, type_tp] <- cali_cost + max(
                        0, st_cali_t[type_tp] - node_cali_cost[node_tp])
                }

                # estimate new movement cost
                # if(l_selected[spot_tp] > 0) {
                #     pair_score_move[node_tp, type_tp] <- move_cost
                # } else {
                if(is.na(spot_score_move[spot_tp])) {
                    # per-spot cali cost is input of estimation
                    spot_cali_cost_tmp <- spot_cali_cost
                    if(multi_cali) {
                        spot_cali_cost_tmp[spot_tp] <-
                            max(spot_cali_cost_tmp[spot_tp],
                                st_cali_t[type_tp])
                    } else {
                        node_cali_cost_tmp <- node_cali_cost
                        node_cali_cost_tmp[node_tp] <-
                            max(node_cali_cost_tmp[node_tp],
                                st_cali_t[type_tp])
                        spot_cali_cost_tmp[spot_tp] <- sum(
                            t(n_location[, spot_tp]) * node_cali_cost_tmp)
                    }

                    # incremental estimation of movement cost
                    single_step_res <- greedy_add_nearest_neighbor(
                        tour_list   = greedy_tour_list,
                        cost_sum    = greedy_cost_sum,
                        unvisited   = spot_tp,
                        distance_matrix = distance_matrix,
                        max_cost_worker = max_cost_worker,
                        spot_cali_cost  = spot_cali_cost_tmp,
                        paranoid    = paranoid
                    )
                    spot_score_move[spot_tp] <- sum(
                        greedy_list_get_multi_tour_len(
                            tour_list       = single_step_res$tour_list,
                            l_selected      = l_selected_tmp,
                            distance_matrix = distance_matrix,
                            start_from_spot = 1L,
                            paranoid        = paranoid # FALSE
                        )
                    )
                }

                # compute/estimate efficiency of new selection
                # pair_score_eval[node_tp, type_tp] <- (weight_overhead +
                #         weight_cali * pair_score_cali[node_tp, type_tp] +
                #         weight_move * pair_score_move[node_tp, type_tp]) /
                #     pair_score_ttni[node_tp, type_tp]
                pair_score_eval[node_tp, type_tp] <- (weight_overhead +
                        weight_cali * pair_score_cali[node_tp, type_tp] +
                        weight_move * spot_score_move[spot_tp]) /
                    pair_score_ttni[node_tp, type_tp]
            }

            if(paranoid) {
                stopifnot(!any(is.na(pair_score_eval)))
                stopifnot(!any(is.na(pair_score_cali)))
              # stopifnot(!any(is.na(pair_score_move)))
                stopifnot(!any(is.na(pair_score_ttni)))
            }

            cp <- which(
                pair_score_eval == min(pair_score_eval),
                arr.ind = TRUE
            )[1L, ] # chosen pair
            node_cp <- cp[1L]
            type_cp <- cp[2L]
            spot_cp <- which(n_location[node_cp, ] > 0)

            if(paranoid) {
                stopifnot(sel[node_cp, type_cp] == 0)
                stopifnot(unsel[node_cp, type_cp])
            }

            sel[node_cp, type_cp] <- 1L
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

            #---------------------------------------------------------------
            # recompute all metrics or update them smartly
            #
            # find selected spots
            l_selected[spot_cp] <- 1L

            # compute time-to-next-iteration based on current plan
            ttnc_after <- get_post_ttnc(
                st_period   = st_period,
                ttnc_before = ttnc_before,
                s_selected  = sel,
                paranoid    = paranoid
            )
            ttni <- min(ttnc_after)

            # if newly added selection does not affect cali cost at its spot
            # then there is no need to recompute cali or move cost
            flag_cali_inc <- NULL
            if(multi_cali) {
                flag_cali_inc <- (st_cali_t[type_cp] > spot_cali_cost[spot_cp])
            } else {
                flag_cali_inc <- (st_cali_t[type_cp] > node_cali_cost[node_cp])
            }
            if(flag_cali_inc) {
                # update calibartion cost
                if(multi_cali) {
                    spot_cali_cost[spot_cp] <-
                        max(spot_cali_cost[spot_cp], st_cali_t[type_cp])
                } else {
                    node_cali_cost[node_cp] <-
                        max(node_cali_cost[node_cp], st_cali_t[type_cp])
                    # spot_cali_cost <-
                    #     rowSums(t(n_location) %*% diag(node_cali_cost))
                    spot_cali_cost[spot_cp] <- sum(
                        t(n_location[, spot_cp]) * node_cali_cost)
                }
                cali_cost <- sum(spot_cali_cost)

                # update movement cost
                greedy_tour_list <- get_multi_paths_greedy_1(
                    l_selected      = l_selected,
                    distance_matrix = distance_matrix,
                    max_cost_worker = max_cost_worker,
                    spot_cali_cost  = spot_cali_cost,
                    raw_return      = TRUE,
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

                # update this info for next loop
                greedy_multi_cali_sum <- greedy_list_get_multi_cali_sum(
                    tour_list       = greedy_tour_list,
                    l_selected      = l_selected,
                    spot_cali_cost  = spot_cali_cost,
                    paranoid        = paranoid
                )
                greedy_cost_sum <- greedy_multi_tour_len + greedy_multi_cali_sum
            }

            # simplification check
            # spot_cali_cost_old <- get_spot_cali_time(
            #     st_cali_t   = st_cali_t,
            #     s_selected  = sel,
            #     n_location  = n_location,
            #     multi_cali  = multi_cali,
            #     paranoid    = paranoid
            # )
            # stopifnot(all(spot_cali_cost == spot_cali_cost_old))

            # compute efficiency of current selection
            plan_effi <- (weight_overhead +
                weight_cali * cali_cost +
                weight_move * move_cost) / ttni
            history_sel[[length(history_sel) + 1L]] <- sel
            history_effi[length(history_effi) + 1L] <- plan_effi

            if(verbose) {
                cat(sprintf("%-35s", paste(
                    sprintf("%8s,", sprintf("(%d)", length(history_sel))),
                    sprintf("sel = %d at %d,", sum(sel), sum(l_selected))
                    )),
                    sprintf("effi = %.3f\n", plan_effi)
                )
            }

            # list out all unselected node-type pairs (i.e. sensors)
            unsel <- (s_presence - sel > 0)
            pairs_unsel <- which(unsel, arr.ind = TRUE)
            colnames(pairs_unsel) <- c("node", "type")
            cnt_unsel <- nrow(pairs_unsel)
        }

        # find the best plan
        best_selection_id <- which(
            history_effi == min(history_effi)
        )[1L]

        if(verbose) {
            cat("Selection algorithm done.\n")
            cat(sprintf("%-35s",
                sprintf("%8s ", sprintf("(%d)", best_selection_id))
                ),
                sprintf("effi = %.3f\n",
                    history_effi[best_selection_id])
            )
            cat("<- ")
        }

        history_sel[[best_selection_id]] # RETURN
    } # RETURN
}

}
