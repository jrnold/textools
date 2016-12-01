context("texnewenv")

test_that("texnewenv works with only required arguments", {
  x <- texnewenv("foo")
  expect_is(x, "texnewenv")
  expect_named(x, c("command", "name", "begin_def", "end_def",
                    "nargs", "default", "starred"))
  expect_equal(x[["command"]], "newenvironment")
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["begin_def"]], character())
  expect_equal(x[["end_def"]], character())
  expect_equal(x[["nargs"]], 0L)
  expect_equal(x[["default"]], character())
  expect_equal(x[["starred"]], FALSE)
})

test_that("texnewenv works with all arguments specified", {
  x <- texnewenv("foo",
                     "the beginning #1",
                     "the end #2",
                     nargs = 2,
                     default = "hello",
                     command = "renewenvironment",
                     starred = TRUE)
  expect_is(x, "texnewenv")
  expect_named(x, c("command", "name", "begin_def", "end_def",
                    "nargs", "default", "starred"))
  expect_equal(x[["command"]], "renewenvironment")
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["begin_def"]], "the beginning #1")
  expect_equal(x[["end_def"]], "the end #2")
  expect_equal(x[["nargs"]], 2L)
  expect_equal(x[["default"]], "hello")
  expect_equal(x[["starred"]], TRUE)
})

test_that("as.character.texnewenv works with only required args", {
  expect_equal(as.character(texnewenv("foo")),
               "\\newenvironment{\\foo}{}{}")
})

test_that("as.character.texnewenv works with all args specified", {
  expect_equal(as.character(texnewenv("foo",
                                      "bar #1",
                                      "baz #2",
                                      nargs = 2,
                                      default = "hello",
                                      command = "renewenvironment",
                                      starred = TRUE)),
               "\\renewenvironment*{\\foo}[2][hello]{bar #1}{baz #2}")
})

test_that("format and as.character methods are equivalent", {
  x <- texnewenv("foo", "bar #1", "baz #2",
                                      nargs = 2,
                                      default = "hello",
                                      command = "renewenvironment",
                                      starred = TRUE)
  expect_equal(as.character(x), format(x))
})


