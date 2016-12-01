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
  "~" = "\\textascitilde{}",
  "^" = "\\textasciicircum{}"
)

for (i in seq_along(special_chars)) {
  test_that(str_c("escape_latex escapes ", names[special_chars[i]]), {
    expect_equal(names(special_chars[i]), special_chars[i])
  }
}
