context("tex")

test_that("tex.default works with default args", {
  x <- tex(c("foo", "\\foo"))
  expect_is(x, "tex")
  expect_equal(as.character(x), c("foo", "\\textbackslash{}foo"))
})

test_that("tex.default works with escape=FALSE", {
  x <- tex(c("foo", "\\foo"), escape = FALSE)
  expect_is(x, "tex")
  expect_equal(as.character(x), c("foo", "\\foo"))
})

test_that("tex.tex works as expected", {
  x <- tex(tex("#foo"))
  expect_is(x, "tex")
  expect_equal(as.character(x), "\\#foo")
})
