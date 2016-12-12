context("Testing Utility Functions")

test_that("brackets works as expected", {
  expect_equal(brackets(c("ab", "c")),
               tex(c("{\\left\\[ab\\right\\]}", "{\\left\\[c\\right\\]}")))
})

test_that("group works as expected", {
  expect_equal(group(c("ab", "c")), tex(c("{ab}", "{c}")))
})

test_that("brackets works with different sizes", {
  expect_equal(brackets(c("ab", "c"), size = NULL),
               tex(c("{\\[ab\\]}", "{\\[c\\]}")))
  expect_equal(brackets(c("ab", "c"), size = ""),
               tex(c("{\\[ab\\]}", "{\\[c\\]}")))
  expect_equal(brackets(c("ab", "c"), size = "\\big"),
               tex(c("{\\big\\[ab\\big\\]}", "{\\big\\[c\\big\\]}")))
})

test_that("parens works as expected", {
  expect_equal(parens(c("ab", "c")),
                      tex(str_c("{\\left(", c("ab", "c"), "\\right)}")))
})

test_that("math() works as expected", {
  expect_equal(math(c("x + y", "z")), tex(c("\\(x + y\\)", "\\(z\\)")))
  expect_equal(math(c("x + y", "z"), TRUE),
               tex(c("\\(x + y\\)", "\\(z\\)")))
  expect_equal(math(c("x + y", "z"), FALSE),
               tex(c("\\[\nx + y\n\\]", "\\[\nz\n\\]")))
  expect_equal(dmath(c("x + y", "z")), math(c("x + y", "z"), inline = FALSE))
  expect_equal(math(c("x + y", "z"), inline = TRUE, dollar = TRUE),
               tex(c("$x + y$", "$z$")))
  expect_equal(math(c("x + y", "z"), inline = FALSE, dollar = TRUE),
               tex(c("$$\nx + y\n$$", "$$\nz\n$$")))
  expect_error(math("a", "b"), regexp = "not a flag")
})

test_that("texcomment() works as expected", {
  expect_equal(texcomment(c("a", "b")), tex(c("% a\n", "% b\n")))
  expect_equal(texcomment(c("a", "b"), newline = TRUE),
               tex(c("% a\n", "% b\n")))
  expect_equal(texcomment(c("a", "b"), newline = FALSE), tex(c("% a", "% b")))
})

test_that("texcomment() throws expected exceptions", {
  expect_error(texcomment("a", "b"), regexp = "not a flag")
})

test_that("newline() works as expected", {
  expect_equal(texnl(c("a", "b")), tex(c("a\\\\\n", "b\\\\\n")))
})
