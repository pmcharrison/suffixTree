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
  x$root <- new_node(as.integer(NA), -Inf)
  x$order_bound <- order_bound
  x$active_nodes <- list() # ordered from smallest context to greatest context
  x$when <- 0L
  x$terminal <- "$"
  class(x) <- "tree"
  reset_active_nodes(x)
  x
}

#' @export
print.tree <- function(x, ...) {
  cat("A suffix tree with", length(as.list(x$root$children)),
      "observed symbols (including terminals)\n")
}

get_root <- function(tree) {
  tree$root
}

get_order_bound <- function(tree) {
  tree$order_bound
}

get_active_nodes <- function(tree) {
  tree$active_nodes
}

reset_active_nodes <- function(tree) {
  tree$active_nodes <- list(tree$root)
}

add_root_to_active_nodes <- function(tree) {
  tree$active_nodes <- c(tree$root, tree$active_nodes)
}

#' Last location
#'
#' Get the last stored location in the tree.
#' This is typically 0 (if no sequences have yet been stored in the tree)
#' or alternatively the last location in the last sequence entered into
#' the tree.
#' @param tree Suffix tree, as produced by \code{new_tree()}.
#' @export
last_location <- function(tree) {
  stopifnot(is(tree, "tree"))
  tree$when
}
