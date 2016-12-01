context("Testing lxtcommand")

test_that("texcmd works", {
  x <- texcmd("foo")
  expect_s3_class(x, "texcmd")
  expect_named(x, c("command", "args", "optargs"))
  expect_equal(x[["command"]], "foo")
  expect_equal(x[["args"]], character())
  expect_equal(x[["optargs"]], character())
})

test_that("texcmd works with additional arguments", {
  x <- texcmd("foo", "arg1", "arg2", "arg3", optargs = c("a", b = "2"))
  expect_s3_class(x, "texcmd")
  expect_named(x, c("command", "args", "optargs"))
  expect_equal(x[["command"]], "foo")
  expect_equal(x[["args"]], c("arg1", "arg2", "arg3"))
  expect_equal(x[["optargs"]], c("a", b = "2"))
})

test_that("as.character.texcmd works", {
  expect_equal(as.character(texcmd("foo")), "\\foo{}")
  expect_equal(as.character(texcmd("foo", "a1", "a2", "a3",
                                   optargs = c("a", b="2", "c", d = TRUE))),
                            "\\foo[a,b=2,c,d=TRUE]{a1}{a2}{a3}")
})

test_that("format and as.character produce the same result", {
  x <- texcmd("foo")
  expect_equal(format(x), as.character(x))
})

test_that("macro works as expected", {
  expect_equal(macro("foo"), "\\foo{}")
})

test_that("texcmd requires a string command", {
  expect_error(texcmd(1), regexp = "not a string")
  expect_error(texcmd(c("a", "b")), regexp = "not a string")
})

test_that("texcmd needs a valid LaTeX macro name", {
  expect_error(texcmd("Alpha1"), regexp = "valid LaTeX command name")
  expect_error(texcmd("234"), regexp = "valid LaTeX command name")
  expect_error(texcmd("A_"), regexp = "valid LaTeX command name")
})