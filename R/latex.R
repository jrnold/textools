### To LaTeX

#' Convert R object to LaTeX
#'
#' @param x An R object to be converted to LaTeX.
#' @param ... Arguments passed to methods.
#' @return An object of class \code{c("latex", "character")}.
#' @export
latex <- function(x, ...) {
  UseMethod("latex")
}

#' @param escape Escape LaTeX using the function \code{\link{escape_latex}}.
#' @export
#' @rdname latex
latex.default <- function(x, escape = TRUE, ...) {
  x <- as.character(x)
  if (escape) {
    x <- escape_latex(x, ...)
  }
  structure(x, class = c("latex", "character"))
}

#' @export
latex.latex <- function(x, ...) x
