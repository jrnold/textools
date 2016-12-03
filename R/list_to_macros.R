# TODO: automatically set to_latex to use toLatex methods or
# define to_latex = toLatex

# TODO: use knitr_print() in some way
# TODO: convert Markdown to LaTeX in some way.

#' Convert a list to LaTeX macros
#'
#' For a list or vector. For elements with non-missing names,
#' the elements are converted to a string and t
#'
#' @param x A list or vector
#' @param prefix Prefix added to names
#' @param collapse If not \code{NULL}, then macros are collapsed to
#'    a single string with \code{collapse} as a seperator.
#' @param ... Arguments passed to \code{\link{texnewcmd}}.
#' @export
#' @examples
#' x <- list(Pval = ".001",
#'           Figure = texcmd("includegraphics", "figure.png"))
#' macros <- list_to_macros(x, prefix="MyProj", collapse="\n")
#' \dontrun{
#' cat(macros, file="macros.tex")
#' }
#' # Then \include{macros.tex} to use them in a TeX document
#'
list_to_macros <- function(x, prefix="", collapse = NULL, ...) {
  assert_that(!is.null(names(x)))
  macronames <- str_c(prefix, names(x))
  assert_that(.valid_macroname(macronames))

  f <- function(name, val, ...) {
    description <- latex(val, ...)
    as.character(texnewcmd(name, description))
  }

  latex(str_c(map2_chr(macronames, unname(x), f, ...),
              collapse = collapse), escape = FALSE)
}
