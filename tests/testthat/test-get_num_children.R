context("get_num_children")

test_that("example", {
  t <- new_tree()
  add_seq(t, c("A", "A", "B", "C", "A", "B"), terminate = TRUE)
  # plot(t)
  expect_equal(get_num_children(t, 0), 4)
  expect_equal(get_num_children(t, 0, exclude_terminals = TRUE), 3)
  t <- new_tree()
  add_seq(t, c("A", "A", "B", "C", "A", "B"), terminate = FALSE)
  expect_equal(get_num_children(t, 1), 1)
  expect_equal(get_num_children(t, 2), 1)
  expect_equal(get_num_children(t, 3), 0)
  expect_equal(get_num_children(t, 4), 0)
})

test_that("example", {
  t <- new_tree()
  add_seq(t, c("A", "B", "C", "A", "B", "D", "A", "B"),
          terminate = FALSE)
  expect_equal(get_num_children(t, 0), 4)
  expect_equal(get_num_children(t, 1), 2)
  expect_equal(get_num_children(t, 2), 2)
})
