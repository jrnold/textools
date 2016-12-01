# TODO: automatically set to_latex to use toLatex methods or
# define to_latex = toLatex
# to_latex.sessionInfo <- toLatex.sessionInfo
# to_latex.citEntry <- toLatex.citEntry

# TODO: use knitr_print() in some way
# TODO: convert Markdown to LaTeX in some way.

# Convert a list to LaTeX macros
#
# @param x A list or vector
# @param to_latex_opts A list of arguments passed to \code{\link{to_latex}}.
# @param ... Arguments passed to \code{\link{texnewcmd}}.
# @export
list_to_macros <- function(x, to_latex_opts = list(), ...) {
  f <- function(name, val, ...) {
    description <- invoke(to_latex, to_latex_opts, x = val)
    as.character(texnewcmd(name, description, ...))
  }
  map(x, f, ...)
}
