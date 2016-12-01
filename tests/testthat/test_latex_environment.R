context("latex_environment")

test_that("ltxenv works with only required args", {
  x <- ltxenv("foo", "bar")
  expect_is(x, "latex_environment")
  expect_named(x, c("name", "content", "args", "optargs"))
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["content"]], "bar")
  expect_equal(x[["args"]], character())
  expect_equal(x[["optargs"]], character())
})

test_that("ltxenv works with all args", {
  x <- ltxenv("foo", c(c("bar", "baz"), "qux"), args = c("a1", "a2"),
              optargs = c("a", "b"="c", "d"))
  expect_is(x, "latex_environment")
  expect_named(x, c("name", "content", "args", "optargs"))
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["content"]], "bar\nbaz\nqux")
  expect_equal(x[["args"]], c("a1", "a2"))
  expect_equal(x[["optargs"]], c("a", "b" = "c", "d"))
})

test_that("ltxenv throws error with non-string name", {
  expect_error(ltxenv(1, ""), regexp = "not a string")
  expect_error(ltxenv(c("a", "b"), ""), regexp = "not a string")
})

test_that("ltxenv throws error with an invalid LaTeX macro name", {
  expect_error(ltxenv("a1", ""), regexp = "valid LaTeX")
})

test_that("ltxenv throws error with non-character optargs", {
  expect_error(ltxenv("foo", "", optargs = TRUE),
               regexp = "not a character vector")
})

test_that("as.character.latex_environment works with only required args", {
  x <- ltxenv("foo", "bar")
  expect_equal(as.character(x),
               "\\begin{foo}bar\\end{foo}")
})

test_that("as.character.latex_environment works with all args", {
  x <- ltxenv("foo", "bar", args = c("a1", "a2"),
              optargs = c("a", "b" = "c", "d"))
  expect_equal(as.character(x),
               "\\begin[a,b=c,d]{foo}{a1}{a2}bar\\end{foo}")
})

test_that("as.character and formt methods produce the same results", {
  x <- ltxenv("foo", "bar", args = c("a1", "a2"),
              optargs = c("a", "b" = "c", "d"))
  expect_equal(as.character(x), format(x))
})

test_that("begin function works", {
  expect_equal(as.character(ltxenv("foo", "bar")), begin("foo", "bar"))
})
