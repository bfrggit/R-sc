# plot_aux.R
#
# Created: 2018-10-08
# Updated: 2019-01-02
#  Author: Charles Zhu
#
if(!exists("EX_PLOT_AUX_R")) {
    EX_PLOT_AUX_R <<- TRUE

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
    "number_nodes",
    "probability_presence",
    "number_sensors"
)
lockBinding("VAR_NAMES", globalenv())

VAR_COLUMN_NAMES <<- c(
    "number_nodes"              = "num_nodes",
    "probability_presence"      = "prob",
    "number_sensors"            = "num_sensors"
)
lockBinding("VAR_COLUMN_NAMES", globalenv())
stopifnot(length(VAR_NAMES) == length(VAR_COLUMN_NAMES))
stopifnot(all(VAR_NAMES == names(VAR_COLUMN_NAMES)))

VAR_PLOT_FILENAME_ENTRIES <<- c(
    "number_nodes"              = "v_nodes",
    "probability_presence"      = "v_prob",
    "number_sensors"            = "v_prob" # new presentation of probability
)
lockBinding("VAR_PLOT_FILENAME_ENTRIES", globalenv())
stopifnot(length(VAR_NAMES) == length(VAR_PLOT_FILENAME_ENTRIES))
stopifnot(all(VAR_NAMES == names(VAR_PLOT_FILENAME_ENTRIES)))

VAR_X_LABELS <<- c(
    "number_nodes"              = "Number of nodes",
    "probability_presence"      = "Probability of sensor presence",
    "number_sensors"            = "Number of sensors"
)
lockBinding("VAR_X_LABELS", globalenv())
stopifnot(length(VAR_NAMES) == length(VAR_X_LABELS))
stopifnot(all(VAR_NAMES == names(VAR_X_LABELS)))

VAL_NAMES <<- c(
    "iteration_overhead",
    "calibration_cost",
    "movement_cost",
    "worker_overhead",
    "sum",
    "selector_time",
    "path_planner_time"
)
lockBinding("VAL_NAMES", globalenv())

VAL_COLUMN_NAMES <<- c(
    "iteration_overhead"        = "weighted_overhead",
    "calibration_cost"          = "weighted_cali",
    "movement_cost"             = "weighted_move",
    "worker_overhead"           = "n_path_per_iter",
    "sum"                       = "weighted_sum",
    "selector_time"             = "sl_time_per_iter",
    "path_planner_time"         = "pp_time_per_iter"
)
lockBinding("VAL_COLUMN_NAMES", globalenv())
stopifnot(length(VAL_NAMES) == length(VAL_COLUMN_NAMES))
stopifnot(all(VAL_NAMES == names(VAL_COLUMN_NAMES)))

VAL_Y_LABELS <<- c(
    "iteration_overhead"        = "Weighted iteration overhead",
    "calibration_cost"          = "Weighted calibration cost",
    "movement_cost"             = "Weighted movement cost",
    "worker_overhead"           = "Average number of workers",
    "sum"                       = "Average cost",
    "selector_time"             = "Selector running time",
    "path_planner_time"         = "Path planner running time"
)
lockBinding("VAL_Y_LABELS", globalenv())
stopifnot(length(VAL_NAMES) == length(VAL_Y_LABELS))
stopifnot(all(VAL_NAMES == names(VAL_Y_LABELS)))

VAL_PLOT_FILENAME_PATTERNS <<- c(
    "iteration_overhead"        = "%s_%s_ovr.pdf",
    "calibration_cost"          = "%s_%s_cal.pdf",
    "movement_cost"             = "%s_%s_mov.pdf",
    "worker_overhead"           = "%s_%s_wkr.pdf",
    "sum"                       = "%s_%s_sum.pdf",
    "selector_time"             = "%s_%s_tsl.pdf",
    "path_planner_time"         = "%s_%s_tpp.pdf"
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
                val         = df_csv[, VAL_COLUMN_NAMES[val]]
            ), by = list(
                selector    = df_csv$selector,
                var         = df_csv[, VAR_COLUMN_NAMES[var]]
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
