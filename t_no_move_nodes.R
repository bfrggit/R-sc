cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

NUM_CASES_USED_TYPES <- 4 # number of sensor type test cases used
NUM_CASES_USED_NODES <- 5 # number of nodal sensor presence test cases used
lockBinding("NUM_CASES_USED_TYPES", globalenv())
lockBinding("NUM_CASES_USED_NODES", globalenv())

NUM_NODES_ALL <- seq(5L, 200L, by = 5L)
SELECTORS <- c("minimal", "all", "nodal")
lockBinding("NUM_NODES_ALL", globalenv())
lockBinding("SELECTORS", globalenv())

NUM_ITERS <- 100L
lockBinding("NUM_ITERS", globalenv())

source("lib/basic.R")
source("lib/naive_sel.R")
source("lib/run_no_move.R")

# prepare data structure to keep simulation results
simu_res_mat <- matrix(
    nrow = 3L # number of distinct selectors to test
            * 2L # intervals and calibration cost
            * length(NUM_NODES_ALL)
            * NUM_CASES_USED_TYPES
            * NUM_CASES_USED_NODES,
    ncol = 6L
)
colnames(simu_res_mat) <-
    c("selector", "metric", "num_nodes", "case_type", "case_node", "value")
simu_res <<- data.frame(simu_res_mat)
dir.create(path = "t_data", showWarnings = FALSE)

jnd_res = 1L
for(jnd_types in 1L:NUM_CASES_USED_TYPES) {
    load(sprintf("prep_RData/t_%03x%02x%02x%02x%03x_unif_%02d.RData",
            10L, 14L, 90L, 30L, 600L, jnd_types))

    for(num_nodes in NUM_NODES_ALL) {
        for(jnd_pres in 1L:NUM_CASES_USED_NODES) {
            cat("Test",
                sprintf("case_t = %d of %d, ", jnd_types, NUM_CASES_USED_TYPES),
                sprintf("num_nodes = %d,", num_nodes),
                sprintf("case_n = %d of %d", jnd_pres, NUM_CASES_USED_NODES),
                "\n"
            )
            load(sprintf("prep_RData/p_%03x%03x%02x_unif_%02d.RData",
                    10L, num_nodes, 50L, jnd_pres))

            sel_f_all <- get_sel_f_all(s_presence = presence)
            sel_f_nodal <- get_sel_f_nodal(s_presence = presence)

            # generate initial TTNC matrix
            # in this test, all sensors are initially new
            ttnc_init <- ifelse(
                presence,
                yes = matrix(
                    rep(st_specs$st_period, NUM_NODES),
                    nrow = NUM_NODES,
                    byrow = TRUE
                ),
                no = Inf
            )

            for(sel_name in SELECTORS) {
                #-----------------------------------------------------------
                # actual function call to run simulator
                #
                res_case <- run_no_move(
                    st_period   = st_specs$st_period,
                    st_cali_t   = st_specs$st_cali_t,
                    s_presence  = presence,
                    ttnc_init   = ttnc_init,
                    num_iters   = NUM_ITERS,
                    selector_f  = get(paste(
                            c("sel_f", sel_name), collapse = "_")),
                    paranoid    = FALSE
                )

                # record results
                simu_res[jnd_res, ] <- c(
                    selector    = sel_name,
                    metric      = "interval_mean",
                    num_nodes   = num_nodes,
                    case_type   = jnd_types,
                    case_node   = jnd_pres,
                    value       = 1000000.0 / sum(res_case$intervals)
                )
                jnd_res <- jnd_res + 1L
                simu_res[jnd_res, ] <- c(
                    selector    = sel_name,
                    metric      = "cali_t_mean",
                    num_nodes   = num_nodes,
                    case_type   = jnd_types,
                    case_node   = jnd_pres,
                    value       = sum(res_case$cali_cost)
                            / sum(res_case$intervals)
                )
                jnd_res <- jnd_res + 1L
                stopifnot(jnd_res <= nrow(simu_res))
            }
        }
    }
}
save.image(file = "t_data/no_move_nodes.RData")
