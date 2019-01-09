#!/usr/bin/env Rscript
#
# prep_types_paris.R
#
# Created: 2019-01-03
# Updated: 2019-01-08
#  Author: Charles Zhu
#
# as always, run this script from the project root

rm(list = ls())

NUM_TYPES = 8L
lockBinding("NUM_TYPES", globalenv())

sensor_type_period <- c(28L, 123L, 61L, 56L, 45L, 42L, 31L, 31L)
sensor_type_cali_t <- c(150L, 15L, 900L, 60L, 60L, 90L, 90L, 90L)
names(sensor_type_period) <- names(sensor_type_cali_t) <-
    c(
        "type_1",
        "noise",
        "air_quality",
        paste("type", as.character(4L:8L), sep = "_")
    )
st_specs <- list()
st_specs$st_period <- sensor_type_period
st_specs$st_cali_t <- sensor_type_cali_t

save(
    NUM_TYPES,
    st_specs,
    file = "scenarios/types_paris.RData"
)
