#!/usr/bin/env Rscript
#
# prep_types_cs3.R
#
# Created: 2019-01-01
# Updated: 2019-01-03
#  Author: Charles Zhu
#
# as always, run this script from the project root

rm(list = ls())

NUM_TYPES = 10L

sensor_type_period <- c(30L, 30L, 30L, 45L, 45L, 60L, 91L, 14L, 30L, 30L)
sensor_type_cali_t <- c(rep(1200L, 6L), 900L, 300L, 1800L, 60L)
names(sensor_type_period) <- names(sensor_type_cali_t) <-
    c(
        paste("gas", as.character(1L:6L), sep = "_"),
        "temperature",
        "light",
        "video",
        "noise"
    )
st_specs <- list()
st_specs$st_period <- sensor_type_period
st_specs$st_cali_t <- sensor_type_cali_t

save(
    NUM_TYPES, st_specs,
    file = "scenarios/types_cs3.RData"
)

sensor_type_period["gas_2"]         <- 10L
sensor_type_period["gas_6"]         <- 7L
sensor_type_period["temperature"]   <- 15L
sensor_type_period["video"]         <- 91L
st_specs$st_period <- sensor_type_period

save(
    NUM_TYPES, st_specs,
    file = "scenarios/types_cs3_fire.RData"
)
