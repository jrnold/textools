context("tex")

test_that("tex() works with default args", {
  x <- tex(c("foo", "\\foo"))
  expect_is(x, "tex")
  expect_equal(as.character(x), c("foo", "\\foo"))
})

test_that("print.tex produces correct output", {
  x <- tex(c("foo", "\\bar"))
  expect_equal(capture_output(print(x)), "<tex>\nfoo\n\\bar")
})

test_that("as_tex.tex works as expected", {
  x <- as_tex(tex("\\foo"))
  expect_is(x, "tex")
  expect_equal(as.character(x), "\\foo")
})

test_that("as_tex.character escapes by default", {
  expect_equal(as_tex("$1.00"), tex("\\$1.00"))
})

test_that("as_tex.default works", {
  expect_equal(as_tex(1), tex("1"))
})
