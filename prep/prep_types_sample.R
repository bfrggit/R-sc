#!/usr/bin/env Rscript
#
# prep_types_sample.R
#
# Created: 2018-08-01
#  Author: Charles Zhu

suppressPackageStartupMessages(require(optparse))

opt_list = list(
    make_option(
        c("-O", "--output_file"),
        action = "store", default = NA, type = "character",
        help = "Output filename"),
    make_option(
        c("-s", "--random_seed"),
        action = "store", default = NA, type = "integer",
        help = "Random seed, default = %default"),
    make_option(
        c("-K", "--num_types"),
        action = "store", default = NA, type = "integer",
        help = "Number of types"),
    make_option(
        c("--period_lower"),
        action = "store", default = 14, type = "integer",
        help = "Minimum calibration period, default = %default"),
    make_option(
        c("--period_upper"),
        action = "store", default = 90, type = "integer",
        help = "Maximum calibration period, default = %default"),
    make_option(
        c("--cali_t_lower"),
        action = "store", default = 30, type = "integer",
        help = "Minimum calibration time, default = %default"),
    make_option(
        c("--cali_t_upper"),
        action = "store", default = 600, type = "integer",
        help = "Maximum calibration time, default = %default")
)
opt_obj = OptionParser(option_list = opt_list)
opt = parse_args(opt_obj)

if(is.na(opt$output_file)) {
    print_help(object = opt_obj)
    stop("Must specify output filename.")
}
if(is.na(opt$num_types)) {
    print_help(object = opt_obj)
    stop("Must specify number of types.")
}

stopifnot(!file.exists(opt$output_file))
stopifnot(file.create(opt$output_file))
stopifnot(opt$num_types > 0L)
stopifnot(opt$period_lower > 0L)
stopifnot(opt$period_upper >= opt$period_lower)
stopifnot(opt$cali_t_lower > 0L)
stopifnot(opt$cali_t_upper >= opt$cali_t_lower)

NUM_TYPES <<- opt$num_types
lockBinding("NUM_TYPES", globalenv())

PERIOD_RANGE <<- opt$period_lower:opt$period_upper
CALI_T_RANGE <<- opt$cali_t_lower:opt$cali_t_upper
lockBinding("PERIOD_RANGE", globalenv())
lockBinding("CALI_T_RANGE", globalenv())

if(!is.na(opt$random_seed)) {
    set.seed(opt$random_seed)
}

source("lib/generator.R")

st_specs <- generate_sensor_types_sample(
    num_types       = NUM_TYPES,
    period_range    = PERIOD_RANGE,
    cali_t_range    = CALI_T_RANGE
)

save(
    NUM_TYPES, PERIOD_RANGE, CALI_T_RANGE, st_specs,
    file = opt$output_file
)
