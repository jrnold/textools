context("texenv")

test_that("texenv_ works with only required args", {
  x <- texenv_("foo", "bar")
  expect_is(x, "texenv")
  expect_named(x, c("name", "content", "args", "optargs"))
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["content"]], "bar")
  expect_equal(x[["args"]], character())
  expect_equal(x[["optargs"]], character())
})

test_that("texenv_ works with all args", {
  x <- texenv_("foo", c(c("bar", "baz"), "qux"), args = c("a1", "a2"),
              optargs = c("a", "b" = "c", "d"))
  expect_is(x, "texenv")
  expect_named(x, c("name", "content", "args", "optargs"))
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["content"]], "bar\nbaz\nqux")
  expect_equal(x[["args"]], c("a1", "a2"))
  expect_equal(x[["optargs"]], c("a", "b" = "c", "d"))
})

test_that("texenv_ throws error with non-string name", {
  expect_error(texenv_(1, ""), regexp = "not a string")
  expect_error(texenv_(c("a", "b"), ""), regexp = "not a string")
})

test_that("texenv_ throws error with an invalid LaTeX command name", {
  expect_error(texenv_("a1", ""), regexp = "valid LaTeX")
})

test_that("texenv_ throws error with non-character optargs", {
  expect_error(texenv_("foo", "", optargs = TRUE),
               regexp = "not a character vector")
})

test_that("as.character method works with only required args", {
  x <- as.character(texenv_("foo", "bar"))
  expect_is(x, "character")
  expect_equal(x, "\\begin{foo}bar\\end{foo}")
})

test_that("as.character method works with all args", {
  x <- texenv_("foo", "bar", args = c("a1", "a2"),
              optargs = c("a", "b" = "c", "d"))
  expect_equal(as.character(x),
               "\\begin[a,b=c,d]{foo}{a1}{a2}bar\\end{foo}")
})

test_that("as.character and format methods produce the same results", {
  x <- texenv_("foo", "bar", args = c("a1", "a2"),
              optargs = c("a", "b" = "c", "d"))
  expect_equal(as.character(x), format(x))
})

test_that("texenv function works", {
  x <- texenv("foo", "bar")
  expect_is(x, "latex")
  expect_equal(as.character(x),
               "\\begin{foo}bar\\end{foo}")
})
