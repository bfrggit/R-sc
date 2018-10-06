# graph_aux.R
#
# Created: 2018-10-04
#  Author: Charles Zhu
#
if(!exists("EX_GRAPH_AUX_R")) {
    EX_GRAPH_AUX_R <<- TRUE

source("lib/basic.R")

# note
# the alternative graph (constructed from the distance matrix)
# is a complete digraph, where the weight of each edge is
#   the length of the shortest path in the original digraph
#   that has the same departure and destination vertices.

# test if a given matrix is a zero path
is_zero_path_matrix <<- function(
    path_matrix,
    paranoid = TRUE
) {
    if(paranoid) {
        stopifnot(is.matrix(path_matrix))
        stopifnot(is.integer(path_matrix))
        stopifnot(nrow(path_matrix) == NUM_SPOTS)
        stopifnot(ncol(path_matrix) == NUM_SPOTS)
        stopifnot(all(path_matrix) >= 0L)
    }

    all(path_matrix == 0L) # RETURN
}

# a valid single path is a subgraph of the alternative graph
# that contains only a directed cycle (i.e. no dead end or sub-tour)
#
# test if a given single path is valid
is_valid_single_path_matrix <<- function(
    path_matrix,                    # each "1" is a taken edge
    must_start_from_spot = NULL,    # int as node index or NULL if not required
    paranoid = TRUE
) {
    stopifnot(is.matrix(path_matrix))

    if(paranoid) {
        stopifnot(is.integer(path_matrix))
        stopifnot(nrow(path_matrix) == NUM_SPOTS)
        stopifnot(ncol(path_matrix) == NUM_SPOTS)
        stopifnot(all(path_matrix) >= 0L)

        if(!is.null(must_start_from_spot)) {
            stopifnot(is.integer(must_start_from_spot))
            stopifnot(must_start_from_spot > 0L &&
                      must_start_from_spot <= NUM_SPOTS)
        }
    }
    stopifnot(all(path_matrix == 0L | path_matrix == 1L))
    if(is_zero_path_matrix(path_matrix, paranoid = FALSE)) { return(TRUE) }

    # it is not allowed to travel from a spot to itself
    for(spot in 1L:NUM_SPOTS) {
        stopifnot(path_matrix[spot, spot] == 0L)
    }

    # now the given matrix is a path matrix
    # next we test if it contains a single cycle
    num_out_edges <- rowSums(path_matrix)
    num_in_edges <- colSums(path_matrix)
    if(any(num_out_edges != num_in_edges)) { return(FALSE) }
    if(any(num_out_edges > 1L)) { return(FALSE) }

    start_from_spot <- must_start_from_spot
    if(!is.null(must_start_from_spot)) {
        if(num_out_edges[must_start_from_spot] != 1L) { return(FALSE) }
    } else {
        start_from_spot <- which(num_out_edges == 1L)[1]
    }

    # expected length of the path is the number of edges
    # detect sub-tour, i.e. make sure
    #   there is only 1 cycle that contains all edges in the path matrix
    expected_len <- sum(path_matrix)
    next_spot <- which(path_matrix[start_from_spot, ] != 0L)
    steps <- 1L
    while(next_spot != start_from_spot) {
        next_spot <- which(path_matrix[next_spot, ] != 0L)
        steps <- steps + 1L
    }
    stopifnot(steps <= expected_len)
    if(steps == 1L) { return(FALSE) }
    if(steps < expected_len) { return(FALSE) }
    TRUE # RETURN
}

is_valid_multi_paths_array <<- function(
    paths_array,                    # three-dimensional, L by L by num_workers
    must_start_from_spot = 1L,      # int as node index
    paranoid = TRUE
) {
    if(paranoid) {
        stopifnot(is.array(paths_array))
        stopifnot(is.integer(paths_array))
        stopifnot(length(dim(paths_array)) == 3L)
        stopifnot(dim(paths_array)[1] == NUM_SPOTS)
        stopifnot(dim(paths_array)[2] == NUM_SPOTS)
        stopifnot(dim(paths_array)[3] > 0L)
        stopifnot(all(paths_array) >= 0L)

        stopifnot(is.integer(must_start_from_spot))
        stopifnot(must_start_from_spot > 0L &&
                  must_start_from_spot <= NUM_SPOTS)
    }

    num_workers <- dim(paths_array)[3]
    for(worker in 1L:num_workers) {
        if(!is_valid_single_path_matrix(
            path_matrix             = paths_array[, , worker],
            must_start_from_spot    = must_start_from_spot,
            paranoid                = FALSE)
        ) { return(FALSE) }
    }

    num_out_edges <- apply(paths_array, MARGIN = 1L, FUN = sum)
    num_in_edges <- apply(paths_array, MARGIN = 2L, FUN = sum)
    if(any(num_out_edges != num_in_edges)) { return(FALSE) }
    if(sum(as.integer(num_out_edges > 1L)) > 1L) { return(FALSE) }
    if(num_out_edges[must_start_from_spot] != num_workers) { return(FALSE) }
    TRUE # RETURN
}

get_move_dist_per_worker <<- function(
    distance_matrix,
    paths_array,
    paranoid = TRUE
) {
    if(paranoid) {
        stopifnot(is.matrix(distance_matrix))
        stopifnot(is.integer(distance_matrix))
        stopifnot(ncol(distance_matrix) == NUM_SPOTS)
        stopifnot(nrow(distance_matrix) == NUM_SPOTS)
        stopifnot(all(distance_matrix) >= 0L)
        stopifnot(
            is_valid_multi_paths_array(
                paths_array             = paths_array,
                must_start_from_spot    = 1L,
                paranoid                = paranoid
            )
        )
    }

    apply(
        paths_array,
        MARGIN = 3L,
        FUN = function(path_matrix) {
            sum(path_matrix * distance_matrix)
        }
    ) # RETURN
}

get_move_dist <<- function(
    ...
) {
    sum(get_move_dist_per_worker(...)) # RETURN
}

get_selected_spots_from_selected_sensors <<- function(
    s_selected, # selected sensors
    n_location, # node location matrix, used to map sensor sel to spot sel
    paranoid = TRUE
) {
    if(paranoid) {
        stopifnot(is.integer(s_selected) && is.matrix(s_selected))
        stopifnot(ncol(s_selected) == NUM_TYPES)
        stopifnot(nrow(s_selected) == NUM_NODES)
        stopifnot(all(s_selected == 0L | s_selected == 1L))

        stopifnot(is.integer(n_location) && is.matrix(n_location))
        stopifnot(ncol(n_location) == NUM_SPOTS_POPULATED)
        stopifnot(nrow(n_location) == NUM_NODES)
        stopifnot(all(n_location == 0L | n_location == 1L))
    }

    l_selected <- apply(
        t(n_location) %*% diag(
            apply(
                s_selected,
                MARGIN = 1L,
                FUN = max
            ) # per-node selection, N by 1
        ), # per-spot-node selection, L by N
        MARGIN = 1L,
        FUN = max
    )
    storage.mode(l_selected) <- "integer"
    l_selected # RETURN
}

} # ENDIF
