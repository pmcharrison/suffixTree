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
take_path <- function(node, value, save = FALSE, when = NULL, terminal = FALSE,
                      exclude_update = FALSE) {
  if (is(node, "empty_node")) return(node)
  if (save && is.null(when))
    stop("if <save> is TRUE then <when> cannot be NULL")
  key <- as.character(value)
  stopifnot(!is.na(key), is.scalar(key))
  novel_symbol <- NULL
  res <- if (is.null(node$children[[key]])) {
    novel_symbol <- TRUE
    if (exclude_update)
      stop("update exclusion should never apply to novel symbols")
    if (save)
      node$children[[key]] <- new_node(value, when, terminal = terminal) else
        EMPTY_NODE
  } else {
    novel_symbol <- FALSE
    child <- node$children[[key]]
    if (save) update_logs(child, when, exclude_update = exclude_update)
    child
  }
  attr(res, "novel_symbol") <- novel_symbol
  res
}

update_logs <- function(node, when, exclude_update) {
  node$log_0[[length(node$log_0) + 1L]] <- when
  if (!exclude_update) node$log_1[[length(node$log_1) + 1L]] <- when
}
