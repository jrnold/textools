context("texkv")

test_that("texkv works as expected", {
  expect_equal(texkv(c("a", "b" = 1), "c", "d" = "foo", "bar" = "{a string}"),
               tex("a,b=1,c,d=foo,bar={a string}"))
})

test_that("texkv .escape=TRUE option works", {
  expect_equal(texkv(c("\\foo", bar = 1), .escape = TRUE),
               tex("\\textbackslash{}foo,bar=1"))
})
