add_symbol <- function(tree, value, save, when) {
  for (j in seq_along(tree$active_nodes)) {
    tree$active_nodes[[j]] <- take_path(node = tree$active_nodes[[j]],
                                        value = value,
                                        save = save,
                                        when = when)
  }
  if (save) tree$when <- when
  trim_active_nodes(tree, save = save)
  add_root_to_active_nodes(tree)
}
