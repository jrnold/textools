#' Convert R object to LaTeX text
#'
#' @param x An R object to be converted to LaTeX.
#' @param ... Arguments passed to methods.
#' @return An object of class \code{c("latex", "character")}. The
#'   \code{"latex"} class is primarily used by other functions in this
#'   package to identify text that is already LaTeX, so it knows not to
#'   escape LaTeX special characters.
#' @seealso \code{\link[utils]{toLatex}}
#' @export
#' @examples
#' latex("Some non-escaped LaTeX code with special symbols like # and $.")
#' latex("Already \\textit{formatted} \\LaTeX text.", escape = FALSE)
#'
latex <- function(x, ...) {
  UseMethod("latex")
}

#' @param escape Escape LaTeX using the function \code{\link{escape_latex}}.
#' @export
#' @rdname latex
latex.default <- function(x, escape = FALSE, ...) {
  x <- as.character(x)
  if (escape) {
    x <- escape_latex(x, ...)
  }
  structure(x, class = c("latex", "character"))
}

#' @export
print.latex <- function(x, ...) {
  cat("<latex>\n")
  print(as.character(x))
  invisible(x)
}

#' @export
latex.latex <- function(x, ...) x

#' @export
latex.list <- function(x, ..., collapse="\n") {
  # recursively apply latex
  latex(str_c(map_chr(x, latex, ...), collapse = collapse),
        escape = FALSE)
}
