cat("Reading data file...", "\n")
load("t_data/no_move_nodes.RData")

library(ggplot2)
library(reshape2)

cat("Processing...", "\n")
sr_interval <- simu_res[simu_res$metric == "interval_mean", ]
sr_int_mean <- aggregate.data.frame(
    x = list(
        value       = sr_interval$value
    ), by = list(
        selector    = sr_interval$selector,
        num_nodes   = sr_interval$num_nodes
    ), FUN = mean
)
sr_int_sd <- aggregate.data.frame(
    x = list(
        value       = sr_interval$value
    ), by = list(
        selector    = sr_interval$selector,
        num_nodes   = sr_interval$num_nodes
    ), FUN = sd
)
sr_cali_t <- simu_res[simu_res$metric == "cali_t_mean", ]
sr_cal_mean <- aggregate.data.frame(
    x = list(
        value       = sr_cali_t$value
    ), by = list(
        selector    = sr_cali_t$selector,
        num_nodes   = sr_cali_t$num_nodes
    ), FUN = mean
)
sr_cal_sd <- aggregate.data.frame(
    x = list(
        value       = sr_cali_t$value
    ), by = list(
        selector    = sr_cali_t$selector,
        num_nodes   = sr_cali_t$num_nodes
    ), FUN = sd
)

# cat("Rendering...", "\n")
# cat("Done", "\n")
