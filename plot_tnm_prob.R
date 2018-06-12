cat("Reading data file...", "\n")
load("t_data/no_move_prob.RData")

library(ggplot2)
library(reshape2)

cat("Processing...", "\n")
sr_interval <- simu_res[simu_res$metric == "interval_mean", ]
sr_int_proc <- aggregate.data.frame(
    x = list(
        value       = sr_interval$value
    ), by = list(
        selector    = sr_interval$selector,
        prob        = sr_interval$prob
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
        prob        = sr_cali_t$prob
    ), FUN = function(x) {
        c(
            mean    = mean(x),
            sd      = sd(x),
            se      = sd(x) * qnorm(0.975) / sqrt(length(x))
        )
    }
)

# cat("Rendering...", "\n")
# cat("Done", "\n")
