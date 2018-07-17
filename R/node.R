new_node <- function(value, when, terminal = FALSE) {
  self <- new.env()
  self$value <- value
  self$log_0 <- list(when) # without update exclusion
  self$log_1 <- list(when) # with update exclusion
  self$children <- new.env()
  class(self) <- c(if (terminal) "terminal",
                   "node")
  self
}

get_value <- function(x) {
  x$value
}

get_children <- function(x) {
  x$children
}

is_node_active <- function(node, tree) {
  any(sapply(tree$active_nodes, function(x) identical(x, node)))
}

is_node <- function(x) {
  is(x, "node")
}

is_terminal_node <- function(x) {
  is(x, "node") && is(x, "terminal")
}
