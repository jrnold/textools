context("texnewcmd")

test_that("texnewcmd_ works with only required arguments", {
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

test_that("texnewcmd_ works with all arguments specified", {
  # nolint start
  x <- texnewcmd("foo", "bar #1 #2", nargs = 2, default = "hello",
                     command = "newcommand", starred = TRUE)
  # nolint end
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
  expect_equal(as.character(texnewcmd_("foo", "bar")),
               "\\providecommand{\\foo}{bar}")
})

# nolint start
test_that("as.character.texnewcmd works with all args specified", {
  expect_equal(as.character(texnewcmd("foo", "bar #1 #2",
                                          nargs = 2,
                                          default = "hello",
                                          command = "newcommand",
                                          starred = TRUE)),
               "\\newcommand*{\\foo}[2][hello]{bar #1 #2}")
})
# nolint end
test_that("format and as.character methods are equivalent", {
  x <- texnewcmd("foo", "bar #1 #2",
                      nargs = 2,
                      default = "hello",
                      command = "newcommand",
                      starred = TRUE)
  expect_equal(as.character(x), format(x))
})

test_that("texnewcmd_ works as expected", {
  x <- texnewcmd_("foo", "bar")
  expect_is(x, "latex")
  expect_equal(as.character(x), "\\providecommand{\\foo}{bar}")
})
