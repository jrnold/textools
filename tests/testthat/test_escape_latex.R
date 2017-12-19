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
                      tex(special_chars[i]))
  })
}

test_that("escape latex works with muliple characters", {
  f <- function(x) {
    # include a few non-numbers to make sure I didn't screw something up.
    tex(str_c("Some text. ", x, " some other text!?*_ ",
              sep = " ", collapse = " "))
  }
  expect_equivalent(f(escape_latex(names(special_chars))),
                    f(special_chars))
})

test_that("escape_latex() escapes URLs", {
  orig <- "The http://cran.r-project.org/ is escaped."
  expected <- tex("The \\url{http://cran.r-project.org/} is escaped.")
  expect_equivalent(escape_latex(orig), expected)
})
