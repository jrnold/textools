context("latex")

test_that("latex.default works with default args", {
  x <- latex(c("foo", "\\foo"))
  expect_is(x, "latex")
  expect_equal(as.character(x), c("foo", "\\textbackslash{}foo"))
})

test_that("latex.default works with escape=FALSE", {
  x <- latex(c("foo", "\\foo"), escape = FALSE)
  expect_is(x, "latex")
  expect_equal(as.character(x), c("foo", "\\foo"))
})

test_that("args are passed to escape_latex", {
  x <- latex(c("foo", "..."), escape = TRUE, ellipses = FALSE)
  expect_equal(as.character(x), c("foo", "..."))
})

test_that("latex.latex works as expected", {
  x <- latex(latex("#foo"))
  expect_is(x, "latex")
  expect_equal(as.character(x), "\\#foo")
})

test_that("latex.list works as expected", {
  x <- latex(list("a", "1", "#foo"))
  expect_is(x, "latex")
  expect_length(x, 1)
  expect_equal(as.character(x), "a\n1\n\\#foo")
})

test_that("latex.list accepts argument collapse", {
  x <- latex(list("a", "1", "#foo"), collapse = "")
  expect_equal(as.character(x), "a1\\#foo")
})
