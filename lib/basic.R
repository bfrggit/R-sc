# basic.R
#
# Created: 2018-06-07
#  Author: Charles Zhu
#
# some functions are borrowed from an earlier project
#
if(!exists("EX_BASIC_R")) {
    EX_BASIC_R <<- TRUE

# RETURN z_ notation strings
# z_nd_str <<- function(str_z, val_n) {
#     stopifnot(is.character(str_z))
#     stopifnot(is.integer(val_n))
#     stopifnot(length(val_n) == 1L)
#     stopifnot(length(str_z) == 1L)
#     stopifnot(val_n > 0L)
#
#     paste(str_z, as.character(1L:val_n), sep = "_") # RETURN
# }

# RETURN z_ notation strings constructed from a list
z_cl_str <<- function(str_z, val_c) {
    stopifnot(is.character(str_z))
    stopifnot(is.integer(val_c))
    stopifnot(length(val_c) > 0L)
    stopifnot(length(str_z) == 1L)

    paste(str_z, as.character(val_c), sep = "_") # RETURN
}

# RETURN z_ notation strings
z_nd_str <<- function(str_z, val_n) {
    stopifnot(is.integer(val_n))
    stopifnot(length(val_n) == 1L)
    stopifnot(val_n > 0L)

    z_cl_str(str_z, 1L:val_n) # RETURN
}

} # ENDIF
