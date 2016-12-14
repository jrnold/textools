context("texarg")

expect_attr <- function(x, which, value) {
  expect_equal(attr(x, which), value)
}

test_that("texarg works with default args", {
  y <- texarg("foo")
  expect_is(y, "texarg")
  expect_equivalent(unclass(y), "foo")
  expect_equal(attr(y, "open"), c("{"))
  expect_equal(attr(y, "close"), c("}"))
})

test_that("texarg works with logical delim", {
  expect_attr(texarg("foo", delim = TRUE), "open", c("{"))
  expect_attr(texarg("foo", delim = TRUE), "close", c("}"))
  expect_attr(texarg("foo", delim = FALSE), "open", c("["))
  expect_attr(texarg("foo", delim = FALSE), "close", c("]"))
})

test_that("texarg works with single char delim", {
  x <- texarg("foo", delim = "|")
  expect_attr(x, "open", "|")
  expect_attr(x, "close", "|")
})

test_that("texarg works with two char delim", {
  x <- texarg("foo", delim = c("<", ">"))
  expect_attr(x, "open", "<")
  expect_attr(x, "close", ">")
})

test_that("texopt produces texarg with square bracket delims", {
  expect_equal(texarg("foo", "["), texopt("foo"))
})

test_that("format.texarg works", {
  expect_equal(format(texarg("foo")), "{foo}")
})

test_that("format.texarg works with arbitrary delimiters", {
  expect_equal(format(texarg("foo", delim = c("<", "?"))), "<foo?")
})

test_that("as.character.texarg is equivalent to format.texarg", {
  expect_equal(format(texarg("foo")), as.character(texarg("foo")))
})

test_that("texargs.character works", {
  expect_equal(format(texargs(texopt("baz"), texarg("foo"), texarg("bar"))),
               "[baz]{foo}{bar}")
})
