# graph_generator.R
#
# Created: 2018-09-24
# Updated: 2018-10-03
#  Author: Charles Zhu
#
if(!exists("EX_GRAPH_GENERATOR_R")) {
    EX_GRAPH_GENERATOR_R <<- TRUE

source("lib/basic.R")

generate_connected_digraph_adj_matrix_random_loop <- function(
    num_spots,
    num_edges,
    rand_gen_f    # func that randomly generates a positive int when called
) {
    stopifnot(is.integer(num_spots))
    stopifnot(num_spots > 1L)

    stopifnot(is.integer(num_edges))
    stopifnot(num_edges >= num_spots)
    stopifnot(num_edges <= num_spots * num_spots - num_spots)

    stopifnot(is.function(rand_gen_f))

    spot_indexes <- 1L:num_spots
    adj_mat <- matrix(0L, nrow = num_spots, ncol = num_spots)
    colnames(adj_mat) <- rownames(adj_mat) <- z_nd_str("spot", num_spots)
    self_edge_indexes <- spot_indexes * (num_spots + 1L) - num_spots

    # generate a random directed loop that contains all spots
    # this is the minimum connected digraph on these spots
    # this is done as a random permutation
    perm = sample(spot_indexes, size = num_spots)
    for(jnd in 1L:(num_spots - 1L)) {
        adj_mat[perm[jnd], perm[jnd + 1L]] = rand_gen_f()
    }
    adj_mat[perm[num_spots], perm[1L]] = rand_gen_f() # close the loop

    # paranoid check
    # stopifnot(is.integer(adj_mat))
    # stopifnot(all(adj_mat[self_edge_indexes] == 0L))
    # stopifnot(all(adj_mat >= 0L))
    # stopifnot(sum(adj_mat > 0L) == num_spots)

    # add extra edges to fulfill the total num of edges
    if(num_edges > num_spots) {
        adj_mat[
            sample(
                setdiff(
                    which(adj_mat <= 0L),
                    self_edge_indexes # avoid spot-to-self edges
                ),
                size = num_edges - num_spots
            )
        ] <- rand_gen_f(size = num_edges - num_spots)
    }

    # paranoid check
    stopifnot(is.integer(adj_mat))
    stopifnot(all(adj_mat[self_edge_indexes] == 0L))
    stopifnot(all(adj_mat >= 0L))
    stopifnot(sum(adj_mat > 0L) == num_edges)

    adj_mat # RETURN
}

library(igraph)

} # ENDIF
