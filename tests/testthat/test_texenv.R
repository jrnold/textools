context("texenv")

test_that("texenv works with only required args", {
  x <- texenv("foo", "bar")
  expect_is(x, "texenv")
  expect_named(x, c("name", "content", "args", "opts"))
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["content"]], tex("bar"))
  expect_equal(x[["args"]], NULL)
  expect_equal(x[["opts"]], NULL)
})

test_that("texenv_ works with all args", {
  x <- texenv("foo", c(c("bar", "baz"), "qux"), args = c("a1", "a2"),
              opts = c("a", "b" = "c", "d"))
  expect_is(x, "texenv")
  expect_named(x, c("name", "content", "args", "opts"))
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["content"]], tex("bar\nbaz\nqux"))
  expect_equal(x[["args"]], texargs(c("a1", "a2")))
  expect_equal(x[["opts"]], texopts(c("a", "b" = "c", "d")))
})

test_that("texenv throws error with non-string name", {
  expect_error(texenv(1, ""), regexp = "not a string")
  expect_error(texenv(c("a", "b"), ""), regexp = "not a string")
})

test_that("texenv throws error with an invalid LaTeX command name", {
  expect_error(texenv("a1", ""), regexp = "valid LaTeX")
})

test_that("texenv_ throws error with non-character optargs", {
  expect_error(texenv("foo", "", opts = TRUE),
               regexp = "is not a character vector")
})

test_that("as.character method works with only required args", {
  x <- as.character(texenv_("foo", "bar"))
  expect_is(x, "character")
  expect_equal(x, "\\begin{foo}bar\\end{foo}")
})

test_that("as.character method works with all args", {
  x <- texenv("foo", "bar", args = c("a1", "a2"),
              opts = c("a", "b" = "c", "d"))
  expect_equal(as.character(x),
               "\\begin{foo}[a, b=c, d]{a1}{a2}bar\\end{foo}")
})

test_that("as.character and format methods produce the same results", {
  x <- texenv("foo", "bar", args = c("a1", "a2"),
              opts = c("a", "b" = "c", "d"))
  expect_equal(as.character(x), format(x))
})

test_that("texenv_ function works", {
  x <- texenv_("foo", "bar")
  expect_is(x, "tex")
  expect_equal(as.character(x),
               "\\begin{foo}bar\\end{foo}")
})
