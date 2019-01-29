#' Take path in suffix tree
#'
#' Takes a path in a suffix tree from a given node to a new node determined by
#' the input value.
#' @param node Object of type \code{node}, corresponding to the current node.
#' @param value Name of the (possibly non-existent) target node.
#' Should be a scalar character.
#' @param save Whether to save the journey into the tree.
#' @param time (Numeric scalar) Ignored if \code{save} is \code{FALSE};
#' provides the timepoint that should be saved into the tree.
#' @param pos (Integerish scalar) Ignored if \code{save} is \code{FALSE};
#' corresponds to the 1-indexed position of the event in the training data
#' (across all sequences).
#' @param terminal (Logical scalar) Whether or not the symbol is a terminal symbol.
#' @param exclude_update (Logical scalar) Whether or not updates should be excluded
#' for this event.
#' @return New node reached by taking this path (NA if no valid node found)
take_path <- function(node, value, save = FALSE,
                      time = NULL,
                      pos = NULL,
                      terminal = FALSE,
                      exclude_update = FALSE) {
  if (is(node, "empty_node")) return(node)
  if (save && (is.null(time) || is.null(pos)))
    stop("if <save> is TRUE then <time> and <pos> cannot be NULL")
  key <- as.character(value)
  stopifnot(!is.na(key), is.scalar(key))
  novel_symbol <- NULL
  res <- if (is.null(node$children[[key]])) {
    novel_symbol <- TRUE
    if (exclude_update)
      stop("update exclusion should never apply to novel symbols")
    if (save)
      node$children[[key]] <- new_node(value,
                                       time = time,
                                       pos = pos,
                                       terminal = terminal) else
        EMPTY_NODE
  } else {
    novel_symbol <- FALSE
    child <- node$children[[key]]
    if (save) update_logs(child,
                          time = time,
                          pos = pos,
                          exclude_update = exclude_update)
    child
  }
  attr(res, "novel_symbol") <- novel_symbol
  res
}

update_logs <- function(node, time, pos, exclude_update) {
  for (log in c("log_0", if (!exclude_update) "log_1")) {
    node[[log]]$time[[length(node[[log]]$time) + 1L]] <- time
    node[[log]]$pos[[length(node[[log]]$pos) + 1L]] <- pos
  }
}
