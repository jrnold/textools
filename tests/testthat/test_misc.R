context("Testing Utility Functions")

test_that("brackets works as expected", {
  expect_equal(brackets(c("ab", "c")), c("[ab]", "[c]"))
})

test_that("braces works as expected", {
  expect_equal(braces(c("ab", "c")), c("{ab}", "{c}"))
})

test_that("parens works as expected", {
  expect_equal(parens(c("ab", "c")), c("(ab)", "(c)"))
})

test_that("math() works as expected", {
  expect_equal(math(c("x + y", "z")), c("\\(x + y\\)", "\\(z\\)"))
  expect_equal(math(c("x + y", "z"), TRUE), c("\\(x + y\\)", "\\(z\\)"))
  expect_equal(math(c("x + y", "z"), FALSE), c("\\[x + y\\]", "\\[z\\]"))
  expect_equal(imath(c("x + y", "z")), math(c("x + y", "z"), TRUE))
  expect_error(math("a", "b"), regexp = "not a flag")
})

test_that("pctcomment() works as expected", {
  expect_equal(pctcomment(c("a", "b")), c("% a\n", "% b\n"))
  expect_equal(pctcomment(c("a", "b"), newline = TRUE), c("% a\n", "% b\n"))
  expect_equal(pctcomment(c("a", "b"), newline = FALSE), c("% a", "% b"))
})

test_that("pctcomment() throws expected exceptions", {
  expect_error(pctcomment("a", "b"), regexp = "not a flag")
})

test_that("newlines() works as expected", {
  expect_equal(newlines(c("a", "b")), "a \\\\\nb \\\\\n")
})

