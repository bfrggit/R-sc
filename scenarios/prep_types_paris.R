#!/usr/bin/env Rscript
#
# prep_types_paris.R
#
# Created: 2019-01-03
#  Author: Charles Zhu
#
# as always, run this script from the project root

rm(list = ls())

NUM_TYPES = 7L

sensor_type_period <- c(60L, 14L, 60L, 45L, 45L, 30L, 30L)
sensor_type_cali_t <- c(15L, 900L, 60L, 60L, 90L, 90L, 150L)
names(sensor_type_period) <- names(sensor_type_cali_t) <-
    c(
        "noise",
        "air_quality",
        paste("type", as.character(3L:7L), sep = "_")
    )
st_specs <- list()
st_specs$st_period <- sensor_type_period
st_specs$st_cali_t <- sensor_type_cali_t

save(
    NUM_TYPES,
    st_specs,
    file = "scenarios/types_paris.RData"
)