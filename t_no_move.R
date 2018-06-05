cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

source("lib/generator.R")

NUM_TYPES <<- 10L
NUM_NODES <<- 50L
lockBinding("NUM_TYPES", globalenv())
lockBinding("NUM_NODES", globalenv())

PERIOD_RANGE <<- 14L:90L
CALI_T_RANGE <<- 30L:600L
PROB <<- 0.5
lockBinding("PERIOD_RANGE", globalenv())
lockBinding("CALI_T_RANGE", globalenv())
lockBinding("PROB", globalenv())

source("lib/runner.R")

NUM_ITERS = 100L
lockBinding("NUM_ITERS", globalenv())
NUM_CASES = 10L
lockBinding("NUM_CASES", globalenv())

df_cost_cali <- data.frame(
    row.names = paste("case", as.character(1L:NUM_CASES), sep = "_"))
df_cost_cali$minimal <- df_cost_cali$all <- df_cost_cali$nodal <-
    rep(NaN, NUM_CASES)


df_cost_iter <- data.frame(
    row.names = paste("case", as.character(1L:NUM_CASES), sep = "_"))
df_cost_iter$minimal <- df_cost_iter$all <- df_cost_iter$nodal <-
    rep(NaN, NUM_CASES)

sel_minimal <- function(
    sensor_type_period,
    sensor_type_cali_t,
    sensor_ttnc_init
) {
    res = ifelse(
        sensor_ttnc_init == min(sensor_ttnc_init),
        yes = 1L,
        no = 0L
    ) # RETURN
}

sel_all <- function(
    sensor_type_period,
    sensor_type_cali_t,
    sensor_ttnc_init
) {
    sensor_presence # RETURN
}

sel_nodal <- function(
    sensor_type_period,
    sensor_type_cali_t,
    sensor_ttnc_init
) {
    res = sel_minimal(
        sensor_type_period,
        sensor_type_cali_t,
        sensor_ttnc_init
    )
    nodal_selection = apply(res, MARGIN = 1L, max)

    for(jnd in 1L:nrow(res)) {
        if(nodal_selection[jnd] > 0L) {
            res[jnd, ] <- sensor_presence[jnd, ]
        }
    }
    res # RETURN
}

set.seed(4)
for(ind in 1L:NUM_CASES) {
    cat("Test", sprintf("case = %d of %d", ind, NUM_CASES), "\n")

    # generate test case
    generate_sensor_types_random(
        num_types    = NUM_TYPES,
        period_range = PERIOD_RANGE,
        cali_t_range = CALI_T_RANGE
    )

    sensor_presence <- generate_sensor_presence(
        num_nodes = NUM_NODES,
        num_types = NUM_TYPES,
        prob      = PROB
    )

    ttnc_init <- ifelse(
        sensor_presence,
        yes = matrix(
            rep(sensor_type_period, NUM_NODES),
            nrow = NUM_NODES,
            byrow = TRUE
        ),
        no = Inf
    )

    # run test case in simulator
    run_no_move(
        sensor_type_period = sensor_type_period,
        sensor_type_cali_t = sensor_type_cali_t,
        sensor_presence    = sensor_presence,
        sensor_ttnc_init   = ttnc_init,
        num_iters          = NUM_ITERS,
        selector           = sel_minimal
    )
    df_cost_cali$minimal[ind] <- mean(cali_cost) / sum(intervals)
    df_cost_iter$minimal[ind] <- 10000.0 / sum(intervals)

    run_no_move(
        sensor_type_period = sensor_type_period,
        sensor_type_cali_t = sensor_type_cali_t,
        sensor_presence    = sensor_presence,
        sensor_ttnc_init   = ttnc_init,
        num_iters          = NUM_ITERS,
        selector           = sel_all
    )
    df_cost_cali$all[ind] <- mean(cali_cost) / sum(intervals)
    df_cost_iter$all[ind] <- 10000.0 / sum(intervals)

    run_no_move(
        sensor_type_period = sensor_type_period,
        sensor_type_cali_t = sensor_type_cali_t,
        sensor_presence    = sensor_presence,
        sensor_ttnc_init   = ttnc_init,
        num_iters          = NUM_ITERS,
        selector           = sel_nodal
    )
    df_cost_cali$nodal[ind] <- mean(cali_cost) / sum(intervals)
    df_cost_iter$nodal[ind] <- 10000.0 / sum(intervals)
}
cat("\n")

print(
    list(
        cost_calibration = df_cost_cali,
        cost_iteration = df_cost_iter
    )
)
