### To LaTeX

LaTeX <- function(x, escape = FALSE, ...) {
  x <- as.character(x)
  if (escape) {
    x <- escape_latex(x, ...)
  }
  structure(x, class = c("LaTeX", "character"))
}

to_latex <- function(x, ...) {
  UseMethod("to_latex")
}

to_latex.default <- function(x, escape = FALSE, ...) {
  # TODO: call toLatex if it exists
  LaTeX(as.character(x), escape = TRUE, ...)
}

to_latex.LaTeX <- identity
