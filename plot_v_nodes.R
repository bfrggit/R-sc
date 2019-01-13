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

source("lib/plot_aux.R")

output_prefix = args[1L]
df_csv <- read_multiple_csv(files = args[2L:argc])

library(ggplot2)
library(reshape2)
library(Cairo)

var_index <- "number_nodes"
cat("Processing...", "\n")
df_ls <- aggregate_df_to_list(df_csv, var = var_index)

source("lib/plot_theme.R")

# create plot objects
plot_ls <- list()
for(val_index in VAL_NAMES) {
    df = df_ls[[val_index]]
    df <- df[df$selector %in% names(scale_names_selector), ]
    plot_ls[[val_index]] <- ggplot(data = df, aes(x = var)) +
        ggplot_theme +
        xlab(VAR_X_LABELS[var_index]) +
        ylab(VAL_Y_LABELS[val_index]) +
        expand_limits(y = 0) +
        geom_line(aes(y = val[, "mean"], color = selector), size = 1) +
        geom_errorbar(
            aes(
                ymin = val[, "mean"] - val[, "se"],
                ymax = val[, "mean"] + val[, "se"],
                color = selector
            ), size = 0.5, alpha = 0.5,
            data = subset(df, var %% 10L == 0L),
            width = (max(df$var) - min(df$var)) * 0.02
        ) + geom_point(
            aes(y = val[, "mean"], color = selector, shape = selector),
            size = 3,
            data = subset(df, var %% 10L == 0L)
        ) + scale_color_selector + scale_shape_selector +
        guides(color = guide_legend(nrow = 2, title.position = "left"))

    if(val_index %in% c("selector_time", "path_planner_time")) {
        plot_ls[[val_index]] <- plot_ls[[val_index]] +
            theme(legend.justification = c(0, 1)) +
            theme(legend.position = c(0, 1))
    }
}

dir.create(path = dirname(sprintf("%s_", output_prefix)), showWarnings = FALSE)

# save to file
for(val_index in VAL_NAMES) {
    cat(sprintf("Rendering: %s", VAL_Y_LABELS[val_index]), "\n")
    ggsave(
        filename = sprintf(
            VAL_PLOT_FILENAME_PATTERNS[val_index],
            output_prefix,
            VAR_PLOT_FILENAME_ENTRIES[var_index]
        ),
        plot = plot_ls[[val_index]], device = cairo_pdf,
        width = 8, height = 5, units = "in", dpi = 300
    )
}
cat("Done!", "\n")
