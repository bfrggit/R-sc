# pp_ga_1.R
#
# Created: 2018-12-11
#  Author: Charles Zhu
#
# mTSP (path planning) solver
# GA solution, single chromosome
# the original file is now replaced with a wrapper of pp_ga_grd_1
#   but the greedy suggestion module is disabled

if(!exists("EX_PP_GA_1_R")) {
    EX_PP_GA_1_R <<- TRUE

source("solution/pp_ga_grd_1.R")

get_multi_paths_ga_1 <<- function(
    ...
) {
    get_multi_paths_ga_grd_1(
        ...,
        greedy_init = FALSE
    )
}

} # ENDIF
