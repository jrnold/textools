context("tex")

test_that("tex() works with default args", {
  x <- tex(c("foo", "\\foo"))
  expect_is(x, "tex")
  expect_equal(as.character(x), c("foo", "\\foo"))
})

test_that("print.tex produces correct output", {
  x <- tex(c("foo", "\\bar"))
  expect_equal(capture_output(print(x)), "foo\n\\bar")
})

test_that("as.tex.tex works as expected", {
  x <- as.tex(tex("\\foo"))
  expect_is(x, "tex")
  expect_equal(as.character(x), "\\foo")
})

test_that("as.tex.character escapes by default", {
  expect_equal(as.tex("$1.00"), tex("\\$1.00"))
})

test_that("as.tex.default works", {
  expect_equal(as.tex(1), tex("1"))
})
