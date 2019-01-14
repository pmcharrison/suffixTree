#' Find timepoints that a given continuation was observed in the tree
#' from the current active node with a given order
#' @param tree A suffix tree as produced by \code{new_tree}.
#' @param order Amount of context (e.g. 0 means no context)
#' @param continuation Continuation of interest (should be a scalar).
#' @param update_excluded Boolean; whether to return update-excluded results.
#' @return \code{NULL} if no continuations found
#' @export
when_continuation <- function(tree, order, continuation,
                              update_excluded = FALSE) {
  id <- order + 1L
  child <- if (id <= length(tree$active_nodes)) {
    tree$active_nodes[[id]]$children[[as.character(continuation)]]
  }
  when_visited(child, update_excluded = update_excluded)
}

when_visited <- function(node, update_excluded) {
  if (is(node, "empty_node")) {
    numeric()
  } else {
    field <- if (update_excluded) "log_1" else "log_0"
    format_log(node[[field]])
  }
}

# times_visited <- function(node, update_excluded) {
#   nrow(when_visited(node, update_excluded))
# }

format_log <- function(x) {
  tibble::tibble(
    pos = as.integer(unlist(x$pos)),
    time = as.numeric(unlist(x$time))
  )
}

#' Find timepoints where the terminal symbol was observed after the same
#' context had just been entered into the tree.
#' @param tree A suffix tree as produced by \code{new_tree}.
#' @param order Amount of context to consider (e.g. 0 means no context).
#' @param update_excluded Boolean; whether to return update-excluded results.
#' @return \code{NULL} if no continuations found
#' @export
when_continuation_terminal <- function(tree, order, update_excluded = FALSE) {
  when_continuation(tree = tree, order = order, continuation = tree$terminal,
                    update_excluded = update_excluded)
}

#' Find timepoints when the current context were seen in the dataset,
#' where the current context is defined by the tree's active nodes.
#' @param tree A suffix tree as produced by \code{new_tree}.
#' @param order Amount of context (e.g. 0 means no context).
#' @param update_excluded Boolean; whether to return update-excluded results.
#' @export
when_context <- function(tree, order, update_excluded = FALSE) {
  id <- order + 1L
  field <- if (update_excluded) "log_1" else "log_0"
  format_log(tree$active_nodes[[id]][[field]])
}

#' #' Tabulate all continuations from the current active node with a given order
#' #' @param tree A suffix tree as produced by \code{new_tree}.
#' #' @param order Amount of context to consider (e.g. 0 means no context).
#' #' @param update_excluded Boolean; whether to return update-excluded results.
#' #' @export
#' when_continuations <- function(tree, order, update_excluded = FALSE,
#'                                include_terminal = TRUE) {
#'   id <- order + 1L
#'   children <- if (id <= length(tree$active_nodes)) {
#'     as.list(tree$active_nodes[[id]]$children)
#'   }
#'   if (!include_terminal) children <- Filter(Negate(is_terminal_node), children)
#'   field <- if (update_excluded) "log_1" else "log_0"
#'   sapply(children, function(x) unlist(x[[field]]), simplify = FALSE)
#' }
