#' Find n-gram occurrences
#'
#' Finds n-gram occurrences in the suffix tree.
#' @param tree Suffix tree
#' @param ngram n-gram to search for, as a numeric or character vector.
#' @return Numeric vector of locations for the n-gram occurrences.
#' @export
when_ngram <- function(tree, ngram) {
  ngram <- as.character(ngram)
  stopifnot(length(ngram) > 0L, !anyNA(ngram))
  node <- get_root(tree)
  for (val in ngram)
    node <- take_path(node = node, value = val, save = FALSE)
  when_visited(node, update_excluded = FALSE)
}

#' Count n-gram occurrences
#'
#' Counts n-gram occurrences in the suffix tree.
#' @param tree Suffix tree
#' @param ngram n-gram to search for, as a numeric or character vector.
#' @return Integer n-gram count.
#' @export
count_ngram <- function(tree, ngram) {
  nrow(when_ngram(tree, ngram))
}
