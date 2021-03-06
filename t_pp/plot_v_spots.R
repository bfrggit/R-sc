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

source("lib/plot_aux_t_pp.R")

output_prefix = args[1L]
df_csv <- read_multiple_csv(files = args[2L:argc])

library(ggplot2)
library(reshape2)
library(Cairo)

var_index <- "number_spots"
cat("Processing...", "\n")
df_ls <- aggregate_df_to_list(df_csv, var = var_index)

source("lib/plot_theme_t_pp.R")

# create plot objects
plot_ls <- list()
plot_scaled_ls <- list()
for(val_index in VAL_NAMES) {
    df = df_ls[[val_index]]
    df <- df[df$path_planner %in% names(scale_names_path_planner), ]
    plot_obj_full <- ggplot(data = df, aes(x = var)) +
        ggplot_theme +
        xlab(VAR_X_LABELS[var_index]) +
        ylab(VAL_Y_LABELS[val_index]) +
        expand_limits(y = 0) +
        geom_line(aes(y = val[, "mean"], color = path_planner), size = 1) +
        geom_point(
            aes(y = val[, "mean"], color = path_planner, shape = path_planner),
            size = 3
        ) + scale_color_path_planner + scale_shape_path_planner +
        guides(color = guide_legend(
                nrow = 3,
                title.position = "top",
                byrow = TRUE
            )
        )

    # generate a scaled version of the same plot to focus on smaller var
    # limits are hard-coded
    scaled_xmax <- 13
    scaled_xmin <- 4
    scaled_yval <- sort(df[df$var == 12, ]$val, decreasing = TRUE)

    if(val_index == "solver_time") {
                scaled_ymax <- scaled_yval[scaled_yval < 500][1]
    } else {    scaled_ymax <- scaled_yval[1] }

    plot_obj_scaled <- plot_obj_full +
        geom_errorbar(
            aes(
                ymin = val[, "mean"] - val[, "se"],
                ymax = val[, "mean"] + val[, "se"],
                color = path_planner
            ), size = 0.5, alpha = 0.5,
            width = (scaled_xmax - scaled_xmin) * 0.02
        ) + coord_cartesian(
            xlim = c(scaled_xmin, scaled_xmax),
            ylim = c(0, scaled_ymax)
        )

    # error bars are added separately for full and scaled plots
    plot_obj_full <- plot_obj_full +
        geom_errorbar(
            aes(
                ymin = val[, "mean"] - val[, "se"],
                ymax = val[, "mean"] + val[, "se"],
                color = path_planner
            ), size = 0.5, alpha = 0.5,
            width = (max(df$var) - min(df$var)) * 0.02
        )

    # add plots to lists
    plot_ls[[val_index]] <- plot_obj_full
    plot_scaled_ls[[val_index]] <- plot_obj_scaled
}

dir.create(path = dirname(sprintf("%s_", output_prefix)), showWarnings = FALSE)

# save to file
for(val_index in VAL_NAMES) {
    cat(sprintf("Rendering: %s", VAL_Y_LABELS[val_index]), "\n")
    ggsave(
        filename = sprintf(
            VAL_PLOT_FILENAME_PATTERNS[val_index],
            sprintf("%s_full", output_prefix),
            VAR_PLOT_FILENAME_ENTRIES[var_index]
        ),
        plot = plot_ls[[val_index]], device = cairo_pdf,
        width = 8, height = 5, units = "in", dpi = 300
    )
    ggsave(
        filename = sprintf(
            VAL_PLOT_FILENAME_PATTERNS[val_index],
            sprintf("%s_scaled", output_prefix),
            VAR_PLOT_FILENAME_ENTRIES[var_index]
        ),
        plot = plot_scaled_ls[[val_index]], device = cairo_pdf,
        width = 8, height = 5, units = "in", dpi = 300
    )
}
cat("Done!", "\n")
