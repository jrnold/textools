context("texcmd")

test_that("texcmd works", {
  x <- texcmd("foo")
  expect_s3_class(x, "texcmd")
  expect_named(x, c("command", "args", "opts"))
  expect_equal(x[["command"]], "foo")
  expect_equal(x[["args"]], NULL)
  expect_equal(x[["opts"]], NULL)
})

test_that("texcmd works with additional arguments", {
  x <- texcmd("foo", c("arg1", "arg2", "arg3"),
              opts = c("a", b = "2"))
  expect_s3_class(x, "texcmd")
  expect_named(x, c("command", "args", "opts"))
  expect_equal(x[["command"]], "foo")
  expect_equal(x[["args"]], texargs(c("arg1", "arg2", "arg3")))
  expect_equal(x[["opts"]], texopts(c("a", b = "2")))
})

test_that("as.character method works", {
  expect_equivalent(as.character(texcmd_("foo")),
                    "\\foo{}")
  expect_equal(as.character(texcmd(c("foo", "a1", "a2", "a3"),
                                   opts = c("a", b = "2", "c", d = TRUE))),
               "\\foo[a,b=2,c,d=TRUE]{a1}{a2}{a3}")
})

test_that("format and as.character methods produce the same result", {
  x <- texcmd("foo")
  expect_equal(format(x), as.character(x))
})

test_that("texcmd works as expected", {
  x <- texcmd_("foo")
  expect_is(x, "tex")
  expect_is(x, "character")
  expect_equivalent(as.character(x), "\\foo{}")
})

test_that("texcmd requires a string command", {
  expect_error(texcmd(1), regexp = "not a string")
  expect_error(texcmd(c("a", "b")), regexp = "not a string")
})

test_that("texcmd needs a valid LaTeX command name", {
  expect_error(texcmd("Alpha1"), regexp = "valid LaTeX command name")
  expect_error(texcmd("234"), regexp = "valid LaTeX command name")
  expect_error(texcmd("A_"), regexp = "valid LaTeX command name")
})
