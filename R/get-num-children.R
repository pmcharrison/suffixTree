#' Get number of children
#'
#' Returns the number of children of a given active node,
#' excluding terminals if requested.
#' @param tree A suffix tree as produced by \code{new_tree}.
#' @param order Amount of context (e.g. 0 means no context).
#' @param exclude_terminals (Logical scalar)
#' Whether to exclude terminal symbols from this count.
#' @return The number of children of the active node with order as specified,
#' possibly including terminal symbols.
#' @export
get_num_children <- function(tree, order, exclude_terminals = FALSE) {
  id <- order + 1L
  if (id > length(tree$active_nodes)) {
    as.integer(NA)
  } else {
    res <- length(tree$active_nodes[[id]]$children)
    if (exclude_terminals)
      res <- res - !is.null(tree$active_nodes[[id]]$children[[tree$terminal]])
    res
  }
}
