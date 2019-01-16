#' @section
#' Update excluded counts, if enabled, are calculated according to
#' Moffat (1990) and Bunton (1997).
#' These counts are constructed as follows:
#' - Start at the highest-order context.
#' - If the symbol is novel, increment count, move to the next-lowest-order node, and repeat.
#' - If the symbol is not novel, increment count and stop.
add_symbol <- function(tree, value, save, time, terminal = FALSE) {
  stopifnot(length(tree$active_nodes) > 0L)
  tree$active_order <- NA
  exclude_update <- FALSE
  # Iterate over active_nodes, starting at highest order and decreasing
  for (j in seq(from = length(tree$active_nodes), to = 1L)) {
    res <- take_path(node = tree$active_nodes[[j]],
                     value = value,
                     save = save,
                     time = time,
                     pos = tree$num_observed + 1L,
                     terminal = terminal,
                     exclude_update = exclude_update)
    if (is.na(tree$active_order) && !is(res, "empty_node")) tree$active_order <- j
    if (!attr(res, "novel_symbol")) exclude_update <- TRUE
    tree$active_nodes[[j]] <- res
  }
  if (save) tree$num_observed <- tree$num_observed + 1L
  trim_active_nodes(tree, save = save)
  add_root_to_active_nodes(tree)
}
