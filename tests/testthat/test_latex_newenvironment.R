context("latex_newenvironment")


test_that("ltxnewenv works with only required arguments", {
  x <- ltxnewenv("foo")
  expect_is(x, "latex_newenvironment")
  expect_named(x, c("command", "name", "begin_def", "end_def",
                    "nargs", "default", "starred"))
  expect_equal(x[["command"]], "providecommand")
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["begin_def"]], character())
  expect_equal(x[["end_def"]], character())
  expect_equal(x[["nargs"]], 0L)
  expect_equal(x[["default"]], NULL)
  expect_equal(x[["starred"]], FALSE)
})

test_that("ltxnewcommand works with all arguments specified", {
  x <- ltxnewenv("foo",
                     "the beginning #1",
                     "the end #2",
                     nargs = 2,
                     default = "hello",
                     command = "newenvironment",
                     starred = TRUE)
  expect_is(x, "latex_newenvironment")
  expect_named(x, c("command", "name", "begin_def", "end_def",
                    "nargs", "default", "starred"))
  expect_equal(x[["command"]], "providecommand")
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["begin_def"]], "tthe beginning #1")
  expect_equal(x[["end_def"]], "the end #2")
  expect_equal(x[["nargs"]], 2L)
  expect_equal(x[["default"]], "hello")
  expect_equal(x[["starred"]], TRUE)
})

test_that("as.character.latex_newcommand works with only required args", {
  expect_equal(as.character(ltxnewenv("foo")),
               "\\newenvironment{\\foo}{}{}")
})

test_that("as.character.latex_newcommand works with all args specified", {
  expect_equal(as.character(ltxnewenv("foo", "bar #1", "baz #2",
                                        nargs = 2,
                                        default = "hello",
                                        command = "renewenvironment",
                                        starred = TRUE)),
               "\\renewenvironemt*{\\foo}[2][hello]{bar #1}{baz #2}")
})

test_that("format and as.character methods are equivalent", {
  x <- ltxnewenv("foo", "bar #1", "baz #2",
                                      nargs = 2,
                                      default = "hello",
                                      command = "renewenvironment",
                                      starred = TRUE)
  expect_equal(as.character(x), format(x))
})

test_that("newenvironment() works as expected", {
  x <- list("foo", "bar #1 #2",
            nargs = 2,
            default = "hello",
            starred = TRUE)
  expect_equal(as.character(do.call(ltxnewcommand,
                                    c(x, list(command = "newcommand")))),
               do.call(newcommand, x))
  expect_equal(as.character(do.call(ltxnewcommand,
                                    c(x, list(command = "renewcommand")))),
               do.call(renewcommand, x))
  expect_equal(as.character(do.call(ltxnewcommand,
                                    c(x, list(command = "providecommand")))),
               do.call(providecommand, x))
})


