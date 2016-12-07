context("texargs")

test_that("texargs() works", {
  xargs <- c("a", "b", "c")
  x <- texargs(args)
  expect_is(x, "texargs")
  expect_equal(unclass(x), args)
})

test_that("texargs() nargs option works when x = NULL", {
  args <- NULL
  nargs <- 2
  x <- texargs(args, nargs = nargs)
  expect_is(x, "texargs")
  expect_equal(unclass(x), rep("", 2))
})

test_that("texargs() nargs option works when x is not NULL", {
  args <- c("a", "b")
  nargs <- 5
  x <- texargs(NULL, nargs = nargs)
  expect_is(x, "texargs")
  expect_equal(unclass(x), c(args, rep("", 3)))
})

test_that("format.texargs works", {
  expect_equal(format(texargs(c("ab", "c"), nargs = 3)), "{ab}{c}{}")
})

test_that("as.character.texargs method works", {
  expect_equal(as.character(texargs(c("ab", "c"), nargs = 3)),
               "{ab}{c}{}")
})

test_that("latex.texargs method works", {
  expect_equal(latex(texargs(c("ab", "c"), nargs = 3)), latex("{ab}{c}{}"))
})
