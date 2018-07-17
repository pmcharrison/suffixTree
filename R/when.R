#' Find timepoints that a given continuation was observed in the tree
#' from the current active node with a given order
#' @param tree A suffix tree as produced by \code{new_tree}.
#' @param order Amount of context (e.g. 0 means no context)
#' @param continuation Continuation of interest (should be a scalar).
#' @return \code{NULL} if no continuations found
#' @export
when_continuation <- function(tree, order, continuation) {
  id <- order + 1L
  child <- if (id <= length(tree$active_nodes)) {
    tree$active_nodes[[id]]$children[[as.character(continuation)]]
  }
  unlist(child$log_0)
}

#' Find timepoints where the terminal symbol was observed after the same
#' context had just been entered into the tree.
#' @param tree A suffix tree as produced by \code{new_tree}.
#' @param order Amount of context to consider (e.g. 0 means no context).
#' @return \code{NULL} if no continuations found
#' @export
when_continuation_terminal <- function(tree, order) {
  when_continuation(tree = tree, order = order, continuation = tree$terminal)
}

#' Tabulate all continuations from the current active node with a given order
#' @param tree A suffix tree as produced by \code{new_tree}.
#' @param order Amount of context to consider (e.g. 0 means no context).
#' @export
when_continuations <- function(tree, order) {
  id <- order + 1L
  children <- if (id <= length(tree$active_nodes)) {
    as.list(tree$active_nodes[[id]]$children)
  }
  sapply(children, function(x) unlist(x$log_0), simplify = FALSE)
}

#' Find timepoints when the current context were seen in the dataset,
#' where the current context is defined by the tree's active nodes.
#' @param tree A suffix tree as produced by \code{new_tree}.
#' @param order Amount of context (e.g. 0 means no context).
#' @export
when_context <- function(tree, order) {
  id <- order + 1L
  unlist(tree$active_nodes[[id]]$log_0)
}
