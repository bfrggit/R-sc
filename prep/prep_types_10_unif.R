# prep_types_10_unif.R
#
# Created: 2018-06-07
#  Author: Charles Zhu

NUM_TYPES <<- 10L
lockBinding("NUM_TYPES", globalenv())

PERIOD_RANGE <<- 14L:90L
CALI_T_RANGE <<- 30L:600L
lockBinding("PERIOD_RANGE", globalenv())
lockBinding("CALI_T_RANGE", globalenv())

num_cases <- 20L
seed <- 4
set.seed(seed)

source("lib/generator.R")

for(jnd in 1L:num_cases) {
    st_specs <- generate_sensor_types_random(
        num_types       = NUM_TYPES,
        period_range    = PERIOD_RANGE,
        cali_t_range    = CALI_T_RANGE
    )

    save(
        NUM_TYPES, PERIOD_RANGE, CALI_T_RANGE, st_specs,
        file = sprintf("prep_RData/t_10_unif_%02d.RData", jnd)
    )
}

