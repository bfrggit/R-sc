cat("Reading data file...", "\n")
load("t_data/no_move_nodes.RData")

library(ggplot2)
library(reshape2)
library(Cairo)

cat("Processing...", "\n")
sr_interval <- simu_res[simu_res$metric == "interval_mean", ]
sr_int_proc <- aggregate.data.frame(
    x = list(
        value       = sr_interval$value
    ), by = list(
        selector    = sr_interval$selector,
        num_nodes   = sr_interval$num_nodes
    ), FUN = function(x) {
        c(
            mean    = mean(x),
            sd      = sd(x),
            se      = sd(x) * qnorm(0.975) / sqrt(length(x))
        )
    }
)
sr_cali_t <- simu_res[simu_res$metric == "cali_t_mean", ]
sr_cal_proc <- aggregate.data.frame(
    x = list(
        value       = sr_cali_t$value
    ), by = list(
        selector    = sr_cali_t$selector,
        num_nodes   = sr_cali_t$num_nodes
    ), FUN = function(x) {
        c(
            mean    = mean(x),
            sd      = sd(x),
            se      = sd(x) * qnorm(0.975) / sqrt(length(x))
        )
    }
)

ggplot_theme <- theme_light() +
    theme(axis.text = element_text(size = 16)) +
    theme(axis.title = element_text(size = 18)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.text = element_text(color = "gray30")) +
    theme(legend.title = element_text(size = 18)) +
    theme(legend.background = element_rect(
        linetype = "solid", color = "gray30")) +
    theme(legend.justification = c(1, 0)) +
    theme(legend.position = c(1, 0))
scale_color_selector <- scale_color_manual(
    name = "Selectors",
    labels = c(
        "minimal"   = "Minimal",
        "all"       = "All",
        "nodal"     = "Nodal"
    ), values = c(
        "minimal"   = "red",
        "all"       = "violetred",
        "nodal"     = "purple"
    )
)
scale_shape_selector <- scale_shape_manual(
    name = "Selectors",
    labels = c(
        "minimal"   = "Minimal",
        "all"       = "All",
        "nodal"     = "Nodal"
    ), values = c(
        "minimal"   = 16,
        "all"       = 18,
        "nodal"     = 17
    )
)

# create plot objects
plot_obj_int <- ggplot(data = sr_int_proc, aes(x = num_nodes)) + ggplot_theme +
    xlab("Number of nodes") +
    ylab("Average iteration overhead") +
    expand_limits(y = 0) +
    geom_line(aes(y = value[, "mean"], color = selector), size = 1) +
    geom_errorbar(
        aes(
            ymin = value[, "mean"] - value[, "se"],
            ymax = value[, "mean"] + value[, "se"],
            color = selector
        ), size = 0.5, alpha = 0.5,
        data = subset(sr_int_proc, num_nodes %% 10L == 0L),
        width = (max(sr_int_proc$num_nodes) - min(sr_int_proc$num_nodes)) * 0.02
    ) + geom_point(
        aes(y = value[, "mean"], color = selector, shape = selector), size = 2,
        data = subset(sr_int_proc, num_nodes %% 10L == 0L)
    ) + scale_color_selector + scale_shape_selector +
    guides(color = guide_legend(nrow = 1, title.position = "left"))

plot_obj_cal <- ggplot(data = sr_cal_proc, aes(x = num_nodes)) + ggplot_theme +
    xlab("Number of nodes") +
    ylab("Average calibration cost") +
    expand_limits(y = 0) +
    geom_line(aes(y = value[, "mean"], color = selector), size = 1) +
    geom_errorbar(
        aes(
            ymin = value[, "mean"] - value[, "se"],
            ymax = value[, "mean"] + value[, "se"],
            color = selector
        ), size = 0.5, alpha = 0.5,
        data = subset(sr_cal_proc, num_nodes %% 10L == 0L),
        width = (max(sr_cal_proc$num_nodes) - min(sr_cal_proc$num_nodes)) * 0.02
    ) + geom_point(
        aes(y = value[, "mean"], color = selector, shape = selector), size = 2,
        data = subset(sr_cal_proc, num_nodes %% 10L == 0L)
    ) + scale_color_selector + scale_shape_selector +
    guides(color = guide_legend(nrow = 1, title.position = "left"))

dir.create(path = "t_plot", showWarnings = FALSE)

# save to file
cat("Rendering: Iteration overhead", "\n")
ggsave(
    filename = "t_plot/no_move_nodes_iter.pdf",
    plot = plot_obj_int,
    device = cairo_pdf,
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)

cat("Rendering: Calibration cost", "\n")
ggsave(
    filename = "t_plot/no_move_nodes_cali.pdf",
    plot = plot_obj_cal,
    device = cairo_pdf,
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)
cat("Done!", "\n")
