context("list_to_macros")

test_that("list_to_macros works as expected", {
  expect_equal(list_to_macros(list("a" = "foo", "b" = "bar")),
               tex(str_c("\\providecommand{\\a}{foo}\n",
                         "\\providecommand{\\b}{bar}")))
})

test_that("list_to_macros throws execption with unnamed elements", {
  expect_error(list_to_macros(list("foo", "b" = "bar")),
               regexp = "All elements of x must be named")
})


test_that("list_to_macros throws execption with invalid macronames", {
  expect_error(list_to_macros(list("foo1" = "foo")),
               regexp = "LaTeX command names can include only letters.")
})
