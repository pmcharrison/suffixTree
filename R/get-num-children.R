#' Get number of children
#'
#' Returns the number of children of a given active node, including terminals.
#' @param tree A suffix tree as produced by \code{new_tree}.
#' @param order Amount of context (e.g. 0 means no context).
#' @return The number of children of the active node with order as specified,
#' including terminal symbols.
#' @export
get_num_children <- function(tree, order) {
  id <- order + 1L
  if (id <= length(tree$active_nodes)) {
    length(tree$active_nodes[[id]]$children)
  } else as.integer(NA)
}
