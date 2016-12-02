# TODO: automatically set to_latex to use toLatex methods or
# define to_latex = toLatex
# to_latex.sessionInfo <- toLatex.sessionInfo
# to_latex.citEntry <- toLatex.citEntry

# TODO: use knitr_print() in some way
# TODO: convert Markdown to LaTeX in some way.

#' Convert a list to LaTeX macros
#'
#' For a list or vector. For elements with non-missing names,
#' the elements are converted to a string and t
#'
#' @param x A list or vector
#' @param latex_opts A list of arguments passed to \code{\link{latex}}
#'    when converting objects to latex.
#' @param prefix Prefix added to names
#' @param ... Arguments passed to \code{\link{texnewcmd}}.
#' @export
list_to_macros <- function(x, latex_opts = list(), prefix="", ...) {
  assert_that(!is.null(names(x)))
  f <- function(name, val, ...) {
    description <- invoke(latex, c(list(val), latex_opts))
    as.character(texnewcmd(name, description))
  }
  macronames <- str_c(prefix, names(x))
  assert_that(.valid_macroname(macronames))
  map2(macronames, x, f, ...)
}

