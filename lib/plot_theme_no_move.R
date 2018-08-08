# plot_theme_no_move.R
#
# Created: 2018-08-08
#  Author: Charles Zhu
#
# Color theme for plots
#
if(!exists("EX_PLOT_THEME_R")) {
    EX_PLOT_THEME_R <<- TRUE

library(ggplot2)

ggplot_theme <<- theme_light() +
    theme(axis.text = element_text(size = 16)) +
    theme(axis.title = element_text(size = 18)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.text = element_text(color = "gray30")) +
    theme(legend.title = element_text(size = 18)) +
    theme(legend.background = element_rect(
        linetype = "solid", color = "gray30")) +
    theme(legend.justification = c(1, 0)) +
    theme(legend.position = c(1, 0))

scale_color_selector <<- scale_color_manual(
    name = "Selectors",
    labels = c(
        "minimal"   = "Minimal",
        "all"       = "All",
        "nodal"     = "Nodal",
        "local"     = "Local"
    ), values = c(
        "minimal"   = "red",
        "all"       = "violetred",
        "nodal"     = "purple",
        "local"     = "blue"
    )
)

scale_shape_selector <<- scale_shape_manual(
    name = "Selectors",
    labels = c(
        "minimal"   = "Minimal",
        "all"       = "All",
        "nodal"     = "Nodal",
        "local"     = "Local"
    ), values = c(
        "minimal"   = 16,
        "all"       = 18,
        "nodal"     = 2,
        "local"     = 17
    )
)

} # ENDIF
