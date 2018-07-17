#' @export
plot.tree <- function(x, wait = FALSE, print = FALSE, shiny = FALSE, ...) {
  spec <- get_graph_spec(x)
  g <- DiagrammeR::create_graph(nodes_df = spec$nodes,
                                edges_df = spec$edges)
  res <- if (shiny) g else DiagrammeR::render_graph(g)
  if (print) print(res)
  if (wait) wait_for_user()
  res
}

get_graph_spec <- function(tree) {
  spec <- new.env()
  spec$nodes <- list(data.frame(value = "ROOT", active = is_node_active(tree$root, tree),
                                stringsAsFactors = FALSE))
  spec$edges <- list()
  add_children_to_graph_spec(spec, node = get_root(tree),
                             node_id = 1L, tree = tree)
  out <- format_graph_spec(spec)
  out
}

format_graph_spec <- function(spec) {
  edges <- as.list(spec)$edges
  if (length(edges) == 0) {
    edges <- NULL
  } else {
    edges <- lapply(edges, function(x) {
      x$log_0 <- paste0("[", paste(x$log_0, collapse = ", "), "]")
      as.data.frame(x, stringsAsFactors = FALSE)
    })
    edges <- do.call(rbind, edges)
    edges <- DiagrammeR::create_edge_df(from = edges$parent, to = edges$child,
                                        label = edges$log_0,
                                        rel = "related")
  }
  nodes <- do.call(rbind, spec$nodes)
  nodes <- DiagrammeR::create_node_df(n = nrow(nodes),
                                      type = "number",
                                      label = as.character(nodes$value),
                                      color = ifelse(nodes$active, "red", "black"))
  list(nodes = nodes, edges = edges)
}

add_children_to_graph_spec <- function(spec, node, node_id, tree) {
  children <- as.list(get_children(node))
  for (child in children) {
    child_node_id <- length(spec$nodes) + 1L
    spec$nodes[[child_node_id]] <- data.frame(value = get_value(child),
                                              active = is_node_active(child, tree),
                                              stringsAsFactors = FALSE)
    spec$edges[[length(spec$edges) + 1L]] <- list(parent = node_id,
                                                  child = child_node_id,
                                                  log_0 = child$log_0)
    add_children_to_graph_spec(spec = spec, node = child, node_id = child_node_id,
                               tree = tree)
  }
}
