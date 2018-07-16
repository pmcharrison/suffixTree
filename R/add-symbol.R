add_symbol <- function(tree, value, save, when) {
  tree$active_order <- 0L
  for (j in seq_along(tree$active_nodes)) {
    res <- take_path(node = tree$active_nodes[[j]],
                                        value = value,
                                        save = save,
                                        when = when)
    if (!is(res, "empty_node")) tree$active_order <- j
    tree$active_nodes[[j]] <- res
  }
  if (save) tree$when <- when
  trim_active_nodes(tree, save = save)
  add_root_to_active_nodes(tree)
}
