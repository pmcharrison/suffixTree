#' Create new suffix tree
#'
#' Creates a new suffix tree that can subsequently be used for sequence modeling.
#' @param order_bound Order bound for the suffix tree, i.e.
#' the largest n-gram that can be stored.
#' @param terminal Reserved terminal symbol; this character must not appear
#' in subsequently modeled input text.
#' @return The new suffix tree.
#' @export
new_tree <- function(order_bound = NULL, terminal = "$") {
  # Inputs ####
  if (!is.null(order_bound) && (!is.integerlike(order_bound) || order_bound < 1))
    stop("invalid order bound")
  terminal <- as.character(terminal)
  if (is.na(terminal) || !is.scalar(terminal))
    stop("invalid terminal")

  # Main ####
  x <- new.env()
  x$root <- new_node(as.integer(NA), pos = 0L, time = 0)
  x$order_bound <- order_bound
  x$active_nodes <- list() # ordered from smallest context to greatest context
  x$active_order <- 0L
  x$num_observed <- 0L
  x$terminal <- "$"
  class(x) <- "tst"
  reset_active_nodes(x)
  x
}

#' @export
is.tst <- function(x) {
  is(x, "tst")
}

#' @export
print.tst <- function(x, ...) {
  order_bound <- if (is.null(x$order_bound)) "none" else x$order_bound
  cat("A temporal suffix tree\n",
      "  - number of stored symbols (inc. terminals) = ", num_observed(x), "\n",
      "  - order bound = ", order_bound, "\n",
      "  - active order = ", x$active_order, "\n",
      sep = "")
}

get_root <- function(tree) {
  stopifnot(is.tst(tree))
  tree$root
}

get_order_bound <- function(tree) {
  stopifnot(is.tst(tree))
  tree$order_bound
}

get_active_nodes <- function(tree) {
  stopifnot(is.tst(tree))
  tree$active_nodes
}

#' Get active order
#'
#' Gets the active order of a suffix tree.
#' The active order is defined as the length of the
#' current conditioning context.
#' When inserting a new sequence, this will typically equal
#' the length of the currently inserted portion,
#' or the tree's order bound (whichever is less).
#' @export
#' @param tree Suffix tree, as produced by \code{new_tree()}.
get_active_order <- function(tree) {
  stopifnot(is.tst(tree))
  tree$active_order
}

#' Reset active nodes
#'
#' Resets the active nodes in a suffix tree.
#' This is typically used in preparation for modeling a new sequence.
#' @param tree Suffix tree, as produced by \code{new_tree()}.
#' @export
reset_active_nodes <- function(tree) {
  stopifnot(is.tst(tree))
  tree$active_nodes <- list(tree$root)
  tree$active_order <- 0L
}

add_root_to_active_nodes <- function(tree) {
  stopifnot(is.tst(tree))
  tree$active_nodes <- c(tree$root, tree$active_nodes)
}

#' Number of observed symbols
#'
#' Returns the number of symbols that have been entered into the tree,
#' including repetitions.
#' @param tree Suffix tree, as produced by \code{new_tree()}.
#' @export
num_observed <- function(tree) {
  stopifnot(is.tst(tree))
  tree$num_observed
}
