# generator.R
#
# Created: 2018-06-04
#  Author: Charles Zhu

if(!exists("EX_GENERATOR_R")) {
    EX_GENERATOR_R <<- TRUE

generate_sensor_types_random <- function(
    num_types,
    period_range,
    cali_t_range
) {
    stopifnot(is.integer(num_types))
    stopifnot(length(num_types) == 1L)
    stopifnot(is.integer(period_range))
    stopifnot(is.integer(cali_t_range))
    stopifnot(length(period_range) >= 1L)
    stopifnot(length(cali_t_range) >= 1L)
    stopifnot(all(period_range) > 0L)
    stopifnot(all(cali_t_range) > 0L)

    sensor_type_period <<- sample(period_range, num_types, replace = TRUE)
    sensor_type_cali_t <<- sample(cali_t_range, num_types, replace = TRUE)
    names(sensor_type_period) <<-names(sensor_type_cali_t) <<-
        paste("type", as.character(1L:num_types), sep = "_")

    NULL # RETURN
}

generate_sensor_presence <- function(
    num_nodes,
    num_types,
    prob
) {
    stopifnot(is.integer(num_nodes))
    stopifnot(length(num_nodes) == 1L)
    stopifnot(is.integer(num_types))
    stopifnot(length(num_types) == 1L)
    stopifnot(is.numeric(prob))
    stopifnot(length(prob) == 1L)
    stopifnot(prob >= 0 & prob <= 1)

    res <- matrix(
        as.integer(runif(num_nodes * num_types) < prob),
        nrow = num_nodes
    )
    colnames(res) = paste("type", as.character(1L:num_types), sep = "_")
    rownames(res) = paste("node", as.character(1L:num_nodes), sep = "_")

    res # RETURN
}

} # ENDIF
