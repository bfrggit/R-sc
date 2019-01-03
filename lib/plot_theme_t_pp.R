# plot_theme_t_pp.R
#
# Created: 2019-01-02
#  Author: Charles Zhu
#
# Color theme for plots
# derived from plot_theme.R
#
if(!exists("EX_PLOT_THEME_T_PP_R")) {
    EX_PLOT_THEME_T_PP_R <<- TRUE

library(ggplot2)

ggplot_theme <<- theme_light() +
    theme(axis.text = element_text(size = 16)) +
    theme(axis.title = element_text(size = 18)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.text = element_text(color = "gray30")) +
    theme(legend.title = element_text(size = 18)) + # element_blank())
    theme(legend.background = element_rect(
        linetype = "solid", color = "gray30")) +
    theme(legend.justification = c(0, 1)) +
    theme(legend.position = c(0, 1))

scale_names_path_planner <<- c(
    "each"              = "Each",
    "ompr_glpk"         = "GLPK",
    "greedy_1"          = "Greedy",
    "ga_grd_1"          = "GA"
)

scale_color_path_planner <<- scale_color_manual(
    name = "Path planners",
    labels = scale_names_path_planner,
    values = c(
        "each"          = "grey",
        "ompr_glpk"     = "navy",
        "greedy_1"      = "orangered",
        "ga_grd_1"      = "olivedrab"
    )
)

scale_shape_path_planner <<- scale_shape_manual(
    name = "Path planners",
    labels = scale_names_path_planner,
    values = c(
        "each"          = 16,
        "ompr_glpk"     = 15,
        "greedy_1"      = 4,
        "ga_grd_1"      = 7
    )
)

} # ENDIF
