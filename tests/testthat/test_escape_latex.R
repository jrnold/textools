context("escape_latex")
library("stringr")

special_chars <- c(
  "{" = "\\{",
  "}" = "\\}",
  "#" = "\\#",
  "$" = "\\$",
  "&" = "\\&",
  "_" = "\\_",
  "%" = "\\%",
  "\\" = "\\textbackslash{}",
  "~" = "\\textasciitilde{}",
  "^" = "\\textasciicircum{}"
)

for (i in seq_along(special_chars)) {
  test_that(str_c("escape_latex escapes ", names(special_chars)[i]), {
    expect_equivalent(escape_latex(names(special_chars)[i]),
                      special_chars[i])
  })
}

test_that("escape latex works with muliple characters", {
  f <- function(x) {
    # include a few non-numbers to make sure I didn't screw something up.
    str_c("Some text. ", x, " some other text!?*_ ", sep = " ", collapse = " ")
  }
  expect_equivalent(f(escape_latex(names(special_chars))),
                    f(special_chars))
})

test_that("escaping ... works", {
  expect_equivalent(escape_latex(" ... "), " \\dots ")
  expect_equivalent(escape_latex(" ... ", ellipses = TRUE), " \\dots ")
  expect_equivalent(escape_latex(" ... ", ellipses = FALSE), " ... ")
})

test_that("escaping ... does no work when not three dots exactly", {
  expect_equivalent(escape_latex("..", ellipses = TRUE), "..")
  expect_equivalent(escape_latex("....", ellipses = TRUE), "....")
})

test_that("escape textbar works", {
  expect_equivalent(escape_latex("|", textbar = TRUE), "\\textbar{}")
  expect_equivalent(escape_latex("|", textbar = FALSE), "|")
})
