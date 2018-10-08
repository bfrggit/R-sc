#!/usr/bin/env Rscript

rm(list = ls())

args        = commandArgs(trailingOnly = TRUE)
args_full   = commandArgs(trailingOnly = FALSE)
argc        = length(args)
argc_full   = length(args_full)

if(argc < 2L) {
    if(argc_full > 0L) {
        write(
            paste(c(
                "Usage:",
                args_full[1L:(argc_full - argc)],
                "OUTPUT_PREFIX",
                "FILE [FILE ...]",
                "\n"
            ), collapse = " "),
            stderr()
        )
    }
    stop("Invalid argument(s)")
}

output_prefix = args[1L]
df_csv = NULL

for(fn in args[2L:argc]) {
    cat(sprintf("Reading data file: %s", fn), "\n")
    df_csv = rbind(
        df_csv,
        read.csv(fn)
    )
}

library(ggplot2)
library(reshape2)
library(Cairo)

cat("Processing...", "\n")
df_agg <- aggregate.data.frame(
    x = list(
        overhead    = df_csv$weighted_overhead,
        cali_t      = df_csv$weighted_cali,
        move_d      = df_csv$weighted_move,
        n_path      = df_csv$n_path_average,
        sum         = df_csv$weighted_sum
    ), by = list(
        selector    = df_csv$selector,
        prob        = df_csv$prob
    ), FUN = function(x) {
        c(
            mean    = mean(x),
            sd      = sd(x),
            se      = sd(x) * qnorm(0.975) / sqrt(length(x))
        )
    }
)

source("lib/plot_theme.R")

# create plot objects
plot_obj_ovr <- ggplot(data = df_agg, aes(x = prob)) + ggplot_theme +
    xlab("Probability of sensor presence") +
    ylab("Weighted iteration overhead") +
    expand_limits(y = 0) +
    geom_line(aes(y = overhead[, "mean"], color = selector), size = 1) +
    geom_errorbar(
        aes(
            ymin = overhead[, "mean"] - overhead[, "se"],
            ymax = overhead[, "mean"] + overhead[, "se"],
            color = selector
        ), size = 0.5, alpha = 0.5,
        # data = subset(df_agg, prob %% 10L == 0L),
        width = (max(df_agg$prob) - min(df_agg$prob)) * 0.02
    ) + geom_point(
        aes(y = overhead[, "mean"], color = selector, shape = selector),
        size = 2
    ) + scale_color_selector + scale_shape_selector +
    guides(color = guide_legend(nrow = 1, title.position = "left"))

plot_obj_cal <- ggplot(data = df_agg, aes(x = prob)) + ggplot_theme +
    xlab("Probability of sensor presence") +
    ylab("Weighted calibration cost") +
    expand_limits(y = 0) +
    geom_line(aes(y = cali_t[, "mean"], color = selector), size = 1) +
    geom_errorbar(
        aes(
            ymin = cali_t[, "mean"] - cali_t[, "se"],
            ymax = cali_t[, "mean"] + cali_t[, "se"],
            color = selector
        ), size = 0.5, alpha = 0.5,
        # data = subset(df_agg, prob %% 10L == 0L),
        width = (max(df_agg$prob) - min(df_agg$prob)) * 0.02
    ) + geom_point(
        aes(y = cali_t[, "mean"], color = selector, shape = selector),
        size = 2
    ) + scale_color_selector + scale_shape_selector +
    guides(color = guide_legend(nrow = 1, title.position = "left"))

plot_obj_mov <- ggplot(data = df_agg, aes(x = prob)) + ggplot_theme +
    xlab("Probability of sensor presence") +
    ylab("Weighted movement cost") +
    expand_limits(y = 0) +
    geom_line(aes(y = move_d[, "mean"], color = selector), size = 1) +
    geom_errorbar(
        aes(
            ymin = move_d[, "mean"] - move_d[, "se"],
            ymax = move_d[, "mean"] + move_d[, "se"],
            color = selector
        ), size = 0.5, alpha = 0.5,
        # data = subset(df_agg, prob %% 10L == 0L),
        width = (max(df_agg$prob) - min(df_agg$prob)) * 0.02
    ) + geom_point(
        aes(y = move_d[, "mean"], color = selector, shape = selector),
        size = 2
    ) + scale_color_selector + scale_shape_selector +
    guides(color = guide_legend(nrow = 1, title.position = "left"))

plot_obj_wkr <- ggplot(data = df_agg, aes(x = prob)) + ggplot_theme +
    xlab("Probability of sensor presence") +
    ylab("Average number of workers") +
    expand_limits(y = 0) +
    geom_line(aes(y = n_path[, "mean"], color = selector), size = 1) +
    geom_errorbar(
        aes(
            ymin = n_path[, "mean"] - n_path[, "se"],
            ymax = n_path[, "mean"] + n_path[, "se"],
            color = selector
        ), size = 0.5, alpha = 0.5,
        # data = subset(df_agg, prob %% 10L == 0L),
        width = (max(df_agg$prob) - min(df_agg$prob)) * 0.02
    ) + geom_point(
        aes(y = n_path[, "mean"], color = selector, shape = selector),
        size = 2
    ) + scale_color_selector + scale_shape_selector +
    guides(color = guide_legend(nrow = 1, title.position = "left"))

plot_obj_sum <- ggplot(data = df_agg, aes(x = prob)) + ggplot_theme +
    xlab("Probability of sensor presence") +
    ylab("Total cost") +
    expand_limits(y = 0) +
    geom_line(aes(y = sum[, "mean"], color = selector), size = 1) +
    geom_errorbar(
        aes(
            ymin = sum[, "mean"] - sum[, "se"],
            ymax = sum[, "mean"] + sum[, "se"],
            color = selector
        ), size = 0.5, alpha = 0.5,
        # data = subset(df_agg, prob %% 10L == 0L),
        width = (max(df_agg$prob) - min(df_agg$prob)) * 0.02
    ) + geom_point(
        aes(y = sum[, "mean"], color = selector, shape = selector),
        size = 2
    ) + scale_color_selector + scale_shape_selector +
    guides(color = guide_legend(nrow = 1, title.position = "left"))

dir.create(path = dirname(sprintf("%s_", output_prefix)), showWarnings = FALSE)

# save to file
cat("Rendering: Iteration overhead", "\n")
ggsave(
    filename = sprintf("%s_v_prob_ovr.pdf", output_prefix),
    plot = plot_obj_ovr,
    device = cairo_pdf,
    width = 8, height = 5, units = "in", dpi = 300
)

cat("Rendering: Calibration cost", "\n")
ggsave(
    filename = sprintf("%s_v_prob_cal.pdf", output_prefix),
    plot = plot_obj_cal,
    device = cairo_pdf,
    width = 8, height = 5, units = "in", dpi = 300
)

cat("Rendering: Movement cost", "\n")
ggsave(
    filename = sprintf("%s_v_prob_mov.pdf", output_prefix),
    plot = plot_obj_mov,
    device = cairo_pdf,
    width = 8, height = 5, units = "in", dpi = 300
)

cat("Rendering: Worker overhead", "\n")
ggsave(
    filename = sprintf("%s_v_prob_wkr.pdf", output_prefix),
    plot = plot_obj_wkr,
    device = cairo_pdf,
    width = 8, height = 5, units = "in", dpi = 300
)

cat("Rendering: Total cost", "\n")
ggsave(
    filename = sprintf("%s_v_prob_sum.pdf", output_prefix),
    plot = plot_obj_sum,
    device = cairo_pdf,
    width = 8, height = 5, units = "in", dpi = 300
)
cat("Done!", "\n")
