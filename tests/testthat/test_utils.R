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
  expect_equal(math(c("x + y", "z"), FALSE),
               c("\\[\nx + y\n\\]", "\\[\nz\n\\]"))
  expect_equal(imath(c("x + y", "z")), math(c("x + y", "z"), TRUE))
  expect_equal(math(c("x + y", "z"), inline = TRUE, dollar = TRUE),
               c("$x + y$", "$z$"))
  expect_equal(math(c("x + y", "z"), inline = FALSE, dollar = TRUE),
               c("$$\nx + y\n$$", "$$\nz\n$$"))
  expect_error(math("a", "b"), regexp = "not a flag")
})

test_that("texcomment() works as expected", {
  expect_equal(texcomment(c("a", "b")), c("% a\n", "% b\n"))
  expect_equal(texcomment(c("a", "b"), newline = TRUE), c("% a\n", "% b\n"))
  expect_equal(texcomment(c("a", "b"), newline = FALSE), c("% a", "% b"))
})

test_that("texcomment() throws expected exceptions", {
  expect_error(texcomment("a", "b"), regexp = "not a flag")
})

test_that("newline() works as expected", {
  expect_equal(newline(c("a", "b")), "a \\\\\nb \\\\\n")
})
