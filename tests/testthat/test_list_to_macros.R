context("list_to_macros")

test_that("list_to_macros works as expected", {
  list_to_macros(list(a = "foo", b = 1))
})
