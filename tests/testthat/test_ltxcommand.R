context("Testing lxtcommand")

test_that("ltxcmd works", {
  x <- ltxcmd("foo")
  expect_s3_class(x, "latex_command")
  expect_named(x, c("command", "args", "optargs"))
  expect_equal(x[["command"]], "foo")
  expect_equal(x[["args"]], character())
  expect_equal(x[["optargs"]], character())
})

test_that("ltxcmd works with additional arguments", {
  x <- ltxcmd("foo", "arg1", "arg2", "arg3", optargs = c("a", b = "2"))
  expect_s3_class(x, "latex_command")
  expect_named(x, c("command", "args", "optargs"))
  expect_equal(x[["command"]], "foo")
  expect_equal(x[["args"]], c("arg1", "arg2", "arg3"))
  expect_equal(x[["optargs"]], c("a", b = "2"))
})

test_that("as.character.latex_command works", {
  expect_equal(as.character(ltxcmd("foo")), "\\foo{}")
  expect_equal(as.character(ltxcmd("foo", "a1", "a2", "a3",
                                   optargs = c("a", b="2", "c", d = TRUE))),
                            "\\foo[a,b=2,c,d=TRUE]{a1}{a2}{a3}")
})

test_that("format and as.character produce the same result", {
  x <- ltxcmd("foo")
  expect_equal(format(x), as.character(x))
})

test_that("macro works as expected", {
  expect_equal(macro("foo"), "\\foo{}")
})

test_that("ltxcmd requires a string command", {
  expect_error(ltxcmd(1), regexp = "not a string")
  expect_error(ltxcmd(c("a", "b")), regexp = "not a string")
})

test_that("ltxcmd needs a valid LaTeX macro name", {
  expect_error(ltxcmd("Alpha1"), regexp = "valid LaTeX command name")
  expect_error(ltxcmd("234"), regexp = "valid LaTeX command name")
  expect_error(ltxcmd("A_"), regexp = "valid LaTeX command name")
})