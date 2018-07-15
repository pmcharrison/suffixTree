#' Take path in suffix tree
#'
#' Takes a path in a suffix tree from a given node to a new node determined by
#' the input value.
#' @param node Object of type \code{node}, corresponding to the current node.
#' @param value Name of the (possibly non-existent) target node.
#' Should be a scalar character.
#' @param save Whether to save the journey into the tree.
#' @param when Ignored if save is FALSE; provides the timepoint that should
#' be saved into the tree.
#' @return New node reached by taking this path (NA if no valid node found)
take_path <- function(node, value, save = FALSE, when = NULL) {
  if (save && is.null(when))
    stop("if <save> is TRUE then <when> cannot be NULL")
  key <- as.character(value)
  stopifnot(!is.na(key), is.scalar(key))
  if (is.null(node$children[[key]])) {
    if (save) node$children[[key]] <- new_node(value, when) else EMPTY_NODE
  } else {
    child <- node$children[[key]]
    if (save) child$log[[length(child$log) + 1L]] <- when
    child
  }
}
