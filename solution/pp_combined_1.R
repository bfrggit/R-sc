# pp_combined_1.R
#
# Created: 2018-12-11
#  Author: Charles Zhu
#
# mTSP (path planning) solver
# this is a wrapper that combines planners we like

if(!exists("EX_PP_COMBINED_1_R")) {
EX_PP_COMBINED_1_R <<- TRUE

source("solution/pp_ompr_glpk.R")
source("solution/pp_ga_grd_1.R")

get_multi_paths_combined_1 <<- function(
    l_selected,
    distance_matrix,
    max_cost_worker,
    spot_cali_cost,
    paranoid
) {
    if(paranoid) {
        stopifnot(is.vector(l_selected))
        stopifnot(is.integer(l_selected))
        stopifnot(all(l_selected >= 0))
    }

    num_selected <- as.integer(sum(l_selected > 0))

    if(num_selected <= 6) {
        glpk_solution <- get_multi_paths_ompr_glpk(
            l_selected      = l_selected,
            distance_matrix = distance_matrix,
            num_workers     = NULL,
            fix_workers     = FALSE,
            max_cost_worker = max_cost_worker,
            spot_cali_cost  = spot_cali_cost,
            paranoid        = paranoid
        )
        if(
            is.array(glpk_solution) && is_valid_multi_paths_array(
                paths_array             = glpk_solution,
                must_start_from_spot    = 1L,
                allow_zero              = FALSE,
                paranoid                = paranoid
            )
        ) { return(glpk_solution) }
    }

    get_multi_paths_ga_grd_1(
        l_selected      = l_selected,
        distance_matrix = distance_matrix,
        max_cost_worker = max_cost_worker,
        spot_cali_cost  = spot_cali_cost,
        ga_seed         = 9L,
        greedy_init     = TRUE,
        paranoid        = paranoid
    ) # RETURN
}

} # ENDIF
