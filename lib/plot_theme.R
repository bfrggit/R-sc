# plot_theme.R
#
# Created: 2018-10-08
# Updated: 2019-01-02
#  Author: Charles Zhu
#
# Color theme for plots
# derived from plot_theme_no_move.R
#
if(!exists("EX_PLOT_THEME_R")) {
    EX_PLOT_THEME_R <<- TRUE

library(ggplot2)

ggplot_theme <<- theme_light() +
    theme(axis.text = element_text(size = 16)) +
    theme(axis.title = element_text(size = 18)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.text = element_text(color = "gray30")) +
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
    # "nodal_lim"         = "Nodal B",
    "local_lim"         = "Local B",
    "interval_1"        = "Interval"
)

scale_color_selector <<- scale_color_manual(
    name = "Selectors",
    labels = scale_names_selector,
    values = c(
        "minimal"       = "brown",
        "all"           = "tomato",
        # "nodal"         = "purple",
        "local"         = "blue",
        # "nodal_lim"     = "mediumpurple",
        "local_lim"     = "skyblue",
        "interval_1"    = "olivedrab"
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
