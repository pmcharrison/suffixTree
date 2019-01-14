#' Add sequence to suffix tree
#'
#' Adds a sequence to a suffix tree. The sequence can be saved to the suffix
#' tree, as when initially building the tree from a corpus (\code{save = TRUE}),
#' or the sequence can be solely tracked in the tree (\code{save = FALSE}),
#' as when e.g. making predictions based on the suffix tree.
#' In the latter case, only the tree's active nodes are updated.
#' @param tree Suffix tree as produced by \code{new_tree}.
#' @param seq Sequence to add, should be a numeric or character vector.
#' @param save Boolean; whether or not to save the sequence in the tree
#' (if \code{save = FALSE}, only the active nodes are updated).
#' @param when Boolean; ignored if \code{save = FALSE}; provides locations
#' for each element of \code{seq}. If \code{NULL} (default), locations
#' are determined as a numeric sequence beginning at the last location
#' entered into the tree, and increasing by 1 each time.
#' @param reset_active_nodes By default, the tree's active nodes are reset
#' before adding the new sequence (\code{reset_active_nodes = TRUE}).
#' Otherwise the new sequence is treated as a continuation of the previous
#' sequence (\code{reset_active_nodes = FALSE}).
#' @param terminate Boolean; ignored if \code{save = FALSE};
#' if \code{TRUE}, the terminal symbol is added to the end of the sequence.
#' @param visual Boolean; whether or not to visualize the tree construction
#' process (this is done interactively using the default plotting device).
#' @return Returns the updated tree. Updates are done in-place,
#' so it is rarely necessary to save the output of this function.
#' @export
#' @examples
#' t <- new_tree()
#' add_seq(t, c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a"), visual = TRUE)
#' t <- new_tree()
#' add_seq(t, sample(5, 5, replace = TRUE))
#' plot(t)
add_seq <- function(tree,
                    seq,
                    save = TRUE,
                    time = NULL,
                    reset_active_nodes = TRUE,
                    terminate = TRUE,
                    visual = FALSE) {
  # Inputs ####
  stopifnot(is(tree, "tree"), is.atomic(seq), is.scalar.logical(save),
            is.null.or(time, is.numeric), is.scalar.logical(reset_active_nodes),
            is.scalar.logical(terminate), is.scalar.logical(visual))
  if (save) {
    if (is.null(time)) {
      time <- seq(from = tree$num_observed,
                       length.out = length(seq))
    } else stopifnot(identical(length(time), length(seq)))
  }

  # Prep ####
  if (reset_active_nodes) reset_active_nodes(tree)
  # Note that we don't reset active_nodes after entering the sequence;
  # this allows when_continuation() to access these active_nodes.
  if (visual) plot(tree, wait = TRUE, print = TRUE)

  # Modelling ####
  for (i in seq_along(seq)) {
    value <- as.character(seq[i])
    if (identical(value, tree$terminal))
      stop("sequence cannot contain terminal character: ", tree$terminal)
    add_symbol(tree, value = value, save = save, time = time[i])
    if (visual) plot(tree, wait = i < length(seq), print = TRUE)
  }

  # Terminal ####
  if (save && terminate) add_symbol(tree,
                                    value = tree$terminal,
                                    save = TRUE,
                                    time = time[i],
                                    terminal = TRUE)
  tree
}
