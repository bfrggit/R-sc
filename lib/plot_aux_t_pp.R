# plot_aux_t_pp.R
#
# Created: 2019-01-02
# Updated: 2019-01-16
#  Author: Charles Zhu
#
if(!exists("EX_PLOT_AUX_T_PP_R")) {
    EX_PLOT_AUX_T_PP_R <<- TRUE

library(ggplot2)

read_multiple_csv <<- function(files) {
    stopifnot(is.character(files))
    stopifnot(length(files) >= 1L)

    df_csv = NULL
    for(fn in files) {
        cat(sprintf("Reading data file: %s", fn), "\n")
        df_csv = rbind(
            df_csv,
            read.csv(fn)
        )
    }
    df_csv # RETURN
}

VAR_NAMES <<- c(
    "number_spots"
)
lockBinding("VAR_NAMES", globalenv())

VAR_COLUMN_NAMES <<- c(
    "number_spots"              = "num_spots"
)
lockBinding("VAR_COLUMN_NAMES", globalenv())
stopifnot(length(VAR_NAMES) == length(VAR_COLUMN_NAMES))
stopifnot(all(VAR_NAMES == names(VAR_COLUMN_NAMES)))

VAR_PLOT_FILENAME_ENTRIES <<- c(
    "number_spots"              = "v_spots"
)
lockBinding("VAR_PLOT_FILENAME_ENTRIES", globalenv())
stopifnot(length(VAR_NAMES) == length(VAR_PLOT_FILENAME_ENTRIES))
stopifnot(all(VAR_NAMES == names(VAR_PLOT_FILENAME_ENTRIES)))

VAR_X_LABELS <<- c(
    "number_spots"              = "Number of spots"
)
lockBinding("VAR_X_LABELS", globalenv())
stopifnot(length(VAR_NAMES) == length(VAR_X_LABELS))
stopifnot(all(VAR_NAMES == names(VAR_X_LABELS)))

VAL_NAMES <<- c(
    "solver_time",
    "number_workers",
    "total_distance"
)
lockBinding("VAL_NAMES", globalenv())

VAL_COLUMN_NAMES <<- c(
    "solver_time"               = "solver_time",
    "number_workers"            = "num_workers",
    "total_distance"            = "total_move_dist"
)
lockBinding("VAL_COLUMN_NAMES", globalenv())
stopifnot(length(VAL_NAMES) == length(VAL_COLUMN_NAMES))
stopifnot(all(VAL_NAMES == names(VAL_COLUMN_NAMES)))

VAL_Y_LABELS <<- c(
    "solver_time"               = "Path planner running time (sec)",
    "number_workers"            = "Number of workers",
    "total_distance"            = "Total movement time (sec)"
)
lockBinding("VAL_Y_LABELS", globalenv())
stopifnot(length(VAL_NAMES) == length(VAL_Y_LABELS))
stopifnot(all(VAL_NAMES == names(VAL_Y_LABELS)))

VAL_PLOT_FILENAME_PATTERNS <<- c(
    "solver_time"               = "%s_%s_tpp.pdf",
    "number_workers"            = "%s_%s_wkr.pdf",
    "total_distance"            = "%s_%s_mov.pdf"
)
lockBinding("VAL_PLOT_FILENAME_PATTERNS", globalenv())
stopifnot(length(VAL_NAMES) == length(VAL_PLOT_FILENAME_PATTERNS))
stopifnot(all(VAL_NAMES == names(VAL_PLOT_FILENAME_PATTERNS)))

aggregate_df_to_list <<- function(df, var) {
    stopifnot(var %in% VAR_NAMES)

    df_ls = list()
    for(val in VAL_NAMES) {
        df_ls[[val]] = aggregate.data.frame(
            x = list(
                val             = df_csv[, VAL_COLUMN_NAMES[val]]
            ), by = list(
                path_planner    = df_csv$path_planner,
                var             = df_csv[, VAR_COLUMN_NAMES[var]]
            ), FUN = function(x) {
                c(
                    mean    = mean(x),
                    sd      = sd(x),
                    se      = sd(x) * qnorm(0.975) / sqrt(length(x))
                )
            }
        )
    }
    df_ls # RETURN
}

} # ENDIF
