context("tex")

test_that("tex() works with default args", {
  x <- LaTeX(c("foo", "\\foo"))
  expect_is(x, "latex")
  expect_equal(as.character(x), c("foo", "\\foo"))
})

test_that("print.latex produces correct output", {
  x <- LaTeX(c("foo", "\\bar"))
  expect_equal(capture_output(print(x)), "foo\n\\bar")
})

test_that("as_latex.latex works as expected", {
  x <- as_latex(LaTeX("\\foo"))
  expect_is(x, "latex")
  expect_equal(as.character(x), "\\foo")
})

test_that("as_latex.character escapes by default", {
  expect_equal(as_latex("$1.00"), LaTeX("\\$1.00"))
})

test_that("as_latex.default works", {
  expect_equal(as_latex(1), LaTeX("1"))
})
