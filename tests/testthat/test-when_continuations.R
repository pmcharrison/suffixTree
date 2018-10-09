context("when_continuations")

test_that("example", {
  t <- new_tree()
  add_seq(t, c("A", "A", "B", "C", "A", "B"), terminate = TRUE)
  add_seq(t, "B", save = FALSE)
  # plot(t)
  expect_equal(
    when_continuations(t, order = 1, update_excluded = FALSE,
                       include_terminal = TRUE),
    list(`$` = 6, C = 4)
  )
  expect_equal(
    when_continuations(t, order = 1, update_excluded = FALSE,
                       include_terminal = FALSE),
    list(C = 4)
  )
})
