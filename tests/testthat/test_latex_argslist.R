context("latex_arglist")

test_that("latex_arglist function works", {
  x <- c("name1" = "a1", "a2", "name3" = "a3")
  arglist <- textools:::latex_arglist(x)
  expect_equal(unclass(arglist), unname(x))
  expect_is(arglist, "latex_arglist")
  expect_is(arglist, "character")
})

test_that("latex_arglist escapes values by default", {
  arglist <- textools:::latex_arglist("a & b")
  expect_equal(unclass(arglist), "a \\& b")
})

test_that("latex_arglist escape=FALSE works", {
  arglist <- textools:::latex_arglist("a & b", escape = FALSE)
  expect_equal(unclass(arglist), "a & b")
})

test_that("as.character.latex_arglist works", {
  x <- textools:::latex_arglist(c("ab", "c"))
  expect_equivalent(textools:::as.character.latex_arglist(x),
                    c("{ab}{c}"))
})

test_that("as.character.latex_arglist works", {
  x <- textools:::latex_arglist(c("ab", "c"))
  expect_equivalent(textools:::latex.latex_arglist(x),
                    latex("{ab}{c}", escape = FALSE))
})
