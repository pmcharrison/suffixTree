trim_active_nodes <- function(tree, save) {
  if (save) {
    # If save = TRUE, trimming is a matter of maintaining the order bound.
    order_bound <- get_order_bound(tree)
    if (!is.null(order_bound)) {
      n <- length(tree$active_nodes)
      if (n > order_bound) stop("something went wrong and order bound was exceeded")
      if (n == order_bound) tree$active_nodes <- tree$active_nodes[- order_bound]
    }
  } else {
    # If save = FALSE, trimming is a matter of removing terminated branches.
    tree$active_nodes <- Filter(function(x) !identical(x, NA),
                                tree$active_nodes)
  }
}
