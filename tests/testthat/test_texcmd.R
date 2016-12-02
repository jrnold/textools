context("texcmd")

test_that("texcmd_ works", {
  x <- texcmd_("foo")
  expect_s3_class(x, "texcmd")
  expect_named(x, c("command", "args", "optargs"))
  expect_equal(x[["command"]], "foo")
  expect_equal(x[["args"]], character())
  expect_equal(x[["optargs"]], character())
})

test_that("texcmd_ works with additional arguments", {
  x <- texcmd_("foo", "arg1", "arg2", "arg3", optargs = c("a", b = "2"))
  expect_s3_class(x, "texcmd")
  expect_named(x, c("command", "args", "optargs"))
  expect_equal(x[["command"]], "foo")
  expect_equal(x[["args"]], c("arg1", "arg2", "arg3"))
  expect_equal(x[["optargs"]], c("a", b = "2"))
})

test_that("as.character method works", {
  expect_equal(as.character(texcmd_("foo")), "\\foo{}")
  expect_equal(as.character(texcmd_("foo", "a1", "a2", "a3",
                                   optargs = c("a", b="2", "c", d = TRUE))),
                            "\\foo[a,b=2,c,d=TRUE]{a1}{a2}{a3}")
})

test_that("format and as.character methods produce the same result", {
  x <- texcmd_("foo")
  expect_equal(format(x), as.character(x))
})

test_that("texcmd works as expected", {
  x <- texcmd("foo")
  expect_is(x, "latex")
  expect_is(x, "character")
  expect_equivalent(x, "\\foo{}")
})

test_that("texcmd_ requires a string command", {
  expect_error(texcmd_(1), regexp = "not a string")
  expect_error(texcmd_(c("a", "b")), regexp = "not a string")
})

test_that("texcmd_ needs a valid LaTeX command name", {
  expect_error(texcmd_("Alpha1"), regexp = "valid LaTeX command name")
  expect_error(texcmd_("234"), regexp = "valid LaTeX command name")
  expect_error(texcmd_("A_"), regexp = "valid LaTeX command name")
})
