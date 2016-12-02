context("texnewcmd")

test_that("texnewcmd works with only required arguments", {
  x <- texnewcmd("foo", "bar")
  expect_is(x, "texnewcmd")
  expect_named(x, c("command", "name", "definition",
                    "nargs", "default", "starred"))
  expect_equal(x[["command"]], "providecommand")
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["definition"]], "bar")
  expect_equal(x[["nargs"]], 0L)
  expect_equal(x[["default"]], NULL)
  expect_equal(x[["starred"]], FALSE)
})

test_that("texnewcmd works with all arguments specified", {
  x <- texnewcmd("foo", "bar #1 #2", nargs = 2, default = "hello",
                     command = "newcommand", starred = TRUE)
  expect_is(x, "texnewcmd")
  expect_named(x, c("command", "name", "definition",
                    "nargs", "default", "starred"))
  expect_equal(x[["command"]], "newcommand")
  expect_equal(x[["name"]], "foo")
  expect_equal(x[["definition"]], "bar #1 #2")
  expect_equal(x[["nargs"]], 2L)
  expect_equal(x[["default"]], "hello")
  expect_equal(x[["starred"]], TRUE)
})

test_that("as.character.texnewcmd works with only required args", {
  expect_equal(as.character(texnewcmd("foo", "bar")),
               "\\providecommand{\\foo}{bar}")
})

test_that("as.character.texnewcmd works with all args specified", {
  expect_equal(as.character(texnewcmd("foo", "bar #1 #2",
                                          nargs = 2,
                                          default = "hello",
                                          command = "newcommand",
                                          starred = TRUE)),
               "\\newcommand*{\\foo}[2][hello]{bar #1 #2}")
})

test_that("format and as.character methods are equivalent", {
  x <- texnewcmd("foo", "bar #1 #2",
                      nargs = 2,
                      default = "hello",
                      command = "newcommand",
                      starred = TRUE)
  expect_equal(as.character(x), format(x))
})

test_that("newcommand() works as expected", {
  x <- list("foo", "bar #1 #2",
             nargs = 2,
             default = "hello",
             starred = TRUE)
  expect_equal(as.character(do.call(texnewcmd,
                                    c(x, list(command = "newcommand")))),
               do.call(newcommand, x))
  expect_equal(as.character(do.call(texnewcmd,
                                    c(x, list(command = "renewcommand")))),
               do.call(renewcommand, x))
  expect_equal(as.character(do.call(texnewcmd,
                                    c(x, list(command = "providecommand")))),
               do.call(providecommand, x))
})
