# plot_theme.R
#
# Created: 2018-10-08
# Updated: 2019-01-16
#  Author: Charles Zhu
#
# Color theme for plots
# derived from plot_theme_no_move.R
#
if(!exists("EX_PLOT_THEME_R")) {
    EX_PLOT_THEME_R <<- TRUE

library(ggplot2)

ggplot_theme <<- theme_light() +
    theme(axis.text = element_text(size = 16, color = "black")) +
    theme(axis.title = element_text(size = 18)) +
    theme(legend.text = element_text(size = 16, color = "black")) +
    theme(legend.title = element_text(size = 18)) + # element_blank())
    theme(legend.background = element_rect(
        linetype = "solid", color = "gray30")) +
    theme(legend.justification = c(1, 0)) +
    theme(legend.position = c(1, 0))

scale_names_selector <<- c(
    "minimal"           = "Minimal",
    "all"               = "All",
    # "nodal"             = "Nodal",
    "local"             = "Local",
    # "nodal_lim"         = "Nodal bnd.",
    "local_lim"         = "Local bnd.",
    "interval_1"        = "TTNI-driven"
)

scale_color_selector <<- scale_color_manual(
    name = "Selectors",
    labels = scale_names_selector,
    values = c(
        "minimal"       = "brown",
        "all"           = "tomato",
        # "nodal"         = "purple",
        "local"         = "mediumpurple",
        # "nodal_lim"     = "blue",
        "local_lim"     = "skyblue",
        "interval_1"    = "black"
    )
)

scale_shape_selector <<- scale_shape_manual(
    name = "Selectors",
    labels = scale_names_selector,
    values = c(
        "minimal"       = 16,
        "all"           = 18,
        # "nodal"         = 2,
        "local"         = 17,
        # "nodal_lim"     = 0,
        "local_lim"     = 15,
        "interval_1"    = 11
    )
)

} # ENDIF
