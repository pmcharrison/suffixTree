% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-seq.R
\name{add_seq}
\alias{add_seq}
\title{Add sequence to suffix tree}
\usage{
add_seq(tree, seq, save = TRUE, time = NULL,
  reset_active_nodes = TRUE, terminate = TRUE, visual = FALSE)
}
\arguments{
\item{tree}{Suffix tree as produced by \code{new_tree}.}

\item{seq}{Sequence to add, should be a numeric or character vector.}

\item{save}{Boolean; whether or not to save the sequence in the tree
(if \code{save = FALSE}, only the active nodes are updated).}

\item{time}{Either \code{NULL} or a numeric vector of timepoints
of the same length as \code{seq}, which should be in ascending order.
If \code{NULL}, then defaults to a sequence in steps of 1
beginning from the number of events already in the tree
(so, for an empty tree: 0, 1, 2, 3, ...).}

\item{reset_active_nodes}{By default, the tree's active nodes are reset
before adding the new sequence (\code{reset_active_nodes = TRUE}).
Otherwise the new sequence is treated as a continuation of the previous
sequence (\code{reset_active_nodes = FALSE}).}

\item{terminate}{Boolean; ignored if \code{save = FALSE};
if \code{TRUE}, the terminal symbol is added to the end of the sequence.}

\item{visual}{Boolean; whether or not to visualize the tree construction
process (this is done interactively using the default plotting device).}
}
\value{
Returns the updated tree. Updates are done in-place,
so it is rarely necessary to save the output of this function.
}
\description{
Adds a sequence to a suffix tree. The sequence can be saved to the suffix
tree, as when initially building the tree from a corpus (\code{save = TRUE}),
or the sequence can be solely tracked in the tree (\code{save = FALSE}),
as when e.g. making predictions based on the suffix tree.
In the latter case, only the tree's active nodes are updated.
}
\examples{
t <- new_tree()
add_seq(t, c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a"), visual = TRUE)
t <- new_tree()
add_seq(t, sample(5, 5, replace = TRUE))
plot(t)
}
