# generator.R
#
# Created: 2018-06-04
# Updated: 2019-01-01
#  Author: Charles Zhu
#
if(!exists("EX_GENERATOR_R")) {
    EX_GENERATOR_R <<- TRUE

generate_sensor_types_sample <- function(
    num_types,
    period_range,
    cali_t_range
) {
    stopifnot(is.integer(num_types))
    stopifnot(length(num_types) == 1L)
    stopifnot(num_types > 0L)

    stopifnot(is.integer(period_range))
    stopifnot(length(period_range) >= 1L)
    stopifnot(all(period_range) > 0L)

    stopifnot(is.integer(cali_t_range))
    stopifnot(length(cali_t_range) >= 1L)
    stopifnot(all(cali_t_range) > 0L)

    # generate the sensor type specifications
    sensor_type_period <- sample(period_range, num_types, replace = TRUE)
    sensor_type_cali_t <- sample(cali_t_range, num_types, replace = TRUE)
    names(sensor_type_period) <- names(sensor_type_cali_t) <-
        paste("type", as.character(1L:num_types), sep = "_")

    res <-list()
    res$st_period <- sensor_type_period
    res$st_cali_t <- sensor_type_cali_t
    res # RETURN
}

# generate each individual sensor with the given probability
generate_sensor_presence_unif <- function(
    num_nodes,
    num_types,
    prob
) {
    stopifnot(is.integer(num_nodes))
    stopifnot(length(num_nodes) == 1L)
    stopifnot(num_nodes > 0L)

    stopifnot(is.integer(num_types))
    stopifnot(length(num_types) == 1L)
    stopifnot(num_types > 0L)

    stopifnot(is.numeric(prob))
    stopifnot(is.finite(prob))
    stopifnot(length(prob) == 1L)
    stopifnot(prob >= 0 & prob <= 1)

    # generate the sensor presence matrix
    res <- matrix(
        as.integer(runif(num_nodes * num_types) < prob),
        nrow = num_nodes
    )
    colnames(res) = paste("type", as.character(1L:num_types), sep = "_")
    rownames(res) = paste("node", as.character(1L:num_nodes), sep = "_")
    res # RETURN
}

# always generate the given total number of sensors
generate_sensor_presence_sample <- function(
    num_nodes,
    num_types,
    num_sensors
) {
    stopifnot(is.integer(num_nodes))
    stopifnot(length(num_nodes) == 1L)
    stopifnot(num_nodes > 0L)

    stopifnot(is.integer(num_types))
    stopifnot(length(num_types) == 1L)
    stopifnot(num_types > 0L)

    stopifnot(is.integer(num_sensors))
    stopifnot(length(num_sensors) == 1L)
    stopifnot(num_sensors > 0L)
    stopifnot(num_sensors <= num_nodes * num_types)

    # generate the sensor presence matrix
    res <- matrix(
        0L,
        nrow = num_nodes,
        ncol = num_types
    )
    res[sample(1L:(num_nodes * num_types), size = num_sensors)] = 1L
    colnames(res) = paste("type", as.character(1L:num_types), sep = "_")
    rownames(res) = paste("node", as.character(1L:num_nodes), sep = "_")
    res # RETURN
}

# always generate the given number of sensors of each type
generate_sensor_presence_sample_per_type <- function(
    num_nodes,
    num_types,
    num_sensors_per_type
) {
    stopifnot(is.integer(num_nodes))
    stopifnot(length(num_nodes) == 1L)
    stopifnot(num_nodes > 0L)

    stopifnot(is.integer(num_types))
    stopifnot(length(num_types) == 1L)
    stopifnot(num_types > 0L)

    stopifnot(is.integer(num_sensors_per_type))
    stopifnot(length(num_sensors_per_type) == num_types)
    stopifnot(all(num_sensors_per_type > 0L))
    stopifnot(all(num_sensors_per_type <= num_nodes))

    # generate the sensor presence matrix
    res <- matrix(
        0L,
        nrow = num_nodes,
        ncol = num_types
    )
    for(tnd in 1L:num_types) {
        res[sample(1L:num_nodes, size = num_sensors_per_type[tnd]), tnd] = 1L
    }
    colnames(res) = paste("type", as.character(1L:num_types), sep = "_")
    rownames(res) = paste("node", as.character(1L:num_nodes), sep = "_")
    res # RETURN
}

conv_location_matrix_to_vector <<- function(
    location_matrix,    # location matrix, each row is a node, col is a spot
    paranoid = TRUE     # enable/disable paranoid checks
) {
    if(paranoid) {
        stopifnot(is.integer(location_matrix) && is.matrix(location_matrix))
        stopifnot(ncol(location_matrix) > 1L)
        stopifnot(nrow(location_matrix) > 0L)
        stopifnot(all(location_matrix == 0L | location_matrix == 1L))
        stopifnot(all(location_matrix[, 1L] == 0L)) # spot 1 should be empty
    }

    num_nodes = nrow(location_matrix)
    num_spots = ncol(location_matrix)
    res <- as.integer(location_matrix %*% matrix(1L:num_spots, ncol = 1L))
    names(res) <- paste("node", as.character(1L:num_nodes), sep = "_")
    res # RETURN
}

conv_location_vector_to_matrix <<- function(
    location_vector,    # location vector, each element refers to a node
    num_spots,          # need this or we do not how many spots we have total
    paranoid = TRUE     # enable/disable paranoid checks
) {
    if(paranoid) {
        stopifnot(is.integer(location_vector))
        stopifnot(length(location_vector) > 0L)
        stopifnot(all(location_vector > 1L & location_vector <= num_spots))
    }

    # generate the node location matrix
    num_nodes = length(location_vector)
    res <- matrix(0L, nrow = num_nodes, ncol = num_spots)
    colnames(res) = paste("spot", as.character(1L:num_spots), sep = "_")
    rownames(res) = paste("node", as.character(1L:num_nodes), sep = "_")
    for(jnd in 1L:num_nodes) {
        res[jnd, location_vector[jnd]] = 1L
    }
    res # RETURN
}

generate_sensor_location_vector_unif <- function(
    num_nodes,
    num_spots
) {
    stopifnot(is.integer(num_nodes))
    stopifnot(length(num_nodes) == 1L)
    stopifnot(num_nodes > 0L)

    stopifnot(is.integer(num_spots))
    stopifnot(length(num_spots) == 1L)
    stopifnot(num_spots > 1L)

	res <- as.integer(sample(2L:num_spots, size = num_nodes, replace = TRUE))
    names(res) <- paste("node", as.character(1L:num_nodes), sep = "_")
    res # RETURN
}

generate_sensor_location_matrix_unif <- function(
    num_nodes,
    num_spots
) {
    conv_location_vector_to_matrix(
        generate_sensor_location_vector_unif(num_nodes, num_spots),
        num_spots = num_spots
    ) # RETURN
}

} # ENDIF
