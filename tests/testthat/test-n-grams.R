context("n-grams")

naive_ngram_counter <- function(source, ngram) {
  UseMethod("naive_ngram_counter", "source")
}

naive_ngram_counter.list <- function(source, ngram) {
  sum(vapply(source, function(x) naive_ngram_counter(x, ngram),
             interger(1)))
}

naive_ngram_counter.numeric <- function(source, ngram) {
  naive_ngram_counter.character(as.character(source), ngram)
}

naive_ngram_counter.character <- function(source, ngram) {
  order <- length(ngram)
  stopifnot(order > 0,
            length(source) >= length(ngram))
  counter <- 0L
  for (i in seq(from = 1, to = length(source) + 1L - order)) {
    if (identical(ngram, source[seq(from = i, length.out = order)]))
      counter <- counter + 1L
  }
  counter
}

test_that("compare to a naive n-gram counter", {
  n <- 1e3
  passage <- sample(c("my", "cat", "ate", "many", "piranhas", "today"),
                    size = n, replace = TRUE)
  tree <- new_tree(order_bound = 5)

  add_seq(tree, passage)
  expect_equal(
    naive_ngram_counter(passage, c("my", "cat")),
    count_ngram(tree, c("my", "cat"))
  )
})

