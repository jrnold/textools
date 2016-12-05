context("expr2latex")

test_that("Greek environment works", {
  expect_equal(expr2latex(pi), "\\pi")
  expect_equal(expr2latex(beta), "\\beta")
})

test_that("all_names works", {
  expect_equal(textools:::all_names(quote(x + y + f(a, b, c, 10))),
               c("x", "y", "a", "b", "c"))
})

test_that("Arbitrary names works", {
  expect_equal(expr2latex(x), "x")
  expect_equal(expr2latex(longvariablename), "longvariablename")
  expect_equal(expr2latex(pi), "\\pi")
})

test_that("Operators work", {
  expect_equal(expr2latex(sin(x + pi)), "\\sin(x + \\pi)")
  expect_equal(expr2latex(log(x_i ^ 2)), "\\log(x_i^2)")
  expect_equal(expr2latex(sin(sin)), "\\sin(sin)")
})

test_that("all_calls() works", {
  expect_equal(textools:::all_calls(quote(f(g + b, c, d(a)))),
               c("f", "+", "d"))
})

test_that("arbitrary functions are recognized", {
  expect_equal(expr2latex(f(a * b)), "\\mathrm{f}(a * b)")
})

test_that("* handles degree, minutes, second", {
  expect_equal(expr2latex(a * b), "a * b")
  expect_equal(expr2latex(a * degree), "a^{\\circ}")
  expect_equal(expr2latex(a * minute), "a'")
  expect_equal(expr2latex(a * second), "a''")
})

test_that("examples from plotmath() work", {
  expect_equal(expr2latex(paste(plain(sin) * phi, " and ", plain(cos) * phi)),
               "\\mathrm{sin} * \\phi  and  \\mathrm{cos} * \\phi")
  # expr2latex should handle quoted args and expressions.
  expect_equal(expr2latex("sin" * phi), "sin * \\phi")
  expect_equal(expr2latex("cos" * phi), "cos * \\phi")
  expect_equal(expr2latex(paste("Phase Angle", phi)), "Phase Angle \\phi")
})
