context("test macros")

test_that("latex_macro creation works with no args", {
  out <- latex_macros()
  expect_is(out, "latex_macros")
  expect_equal(length(out), 0L)
})

test_that("latex_macros creation works with args", {
  out <- latex_macros(a = 1, b = 2, c = 3)
  expect_is(out, "latex_macros")
  expect_equal(length(out), 3L)
})


test_that("latex_macros requires named args", {
  expect_error(latex_macros(a = 1, 2),
               regexp = "All arguments must be named")
})


test_that("latex_macros requires valid names", {
  expect_error(latex_macros(`a1` = 1),
               regexp = "All names must be valid LaTeX command names")
})

test_that("format.latex_macros works", {
  out <- format(latex_macros(a = 1, b = "2 \\ 3", c = "% $"))
  expected <- tex("\\providecommand{\\c}{\\textbackslash{}\\% \\textbackslash{}\\$}\n\\providecommand{\\b}{2 \\textbackslash{}textbackslash\\{\\} 3}\n\\providecommand{\\a}{1}")
  expect_equal(out, expected)
})

test_that("[[<-.latex_macros works", {
  x <- latex_macros(a = 1)
  x[["b"]] <- "foo"
  expect_equal(x[["b"]], as_tex("foo"))
})

test_that("[[<-.latex_macros with NULL deletes values", {
  x <- latex_macros(a = 1)
  x[["a"]] <- NULL
  expect_false("a" %in% ls(x))
})

test_that("[[<-.latex_macros checks validity of names", {
  x <- latex_macros(a = 1)
  x[["b"]] <- "foo"
  expect_equal(x[["b"]], as_tex("foo"))
})

