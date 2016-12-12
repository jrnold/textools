#' Convert a list to LaTeX macros
#'
#' For a list or vector. For elements with non-missing names,
#' the elements are converted to a string and t
#'
#' @param x A list or vector
#' @param prefix Prefix added to names
#' @param collapse If not \code{NULL}, then macros are collapsed to
#'    a single string with \code{collapse} as a seperator.
#' @param ... Arguments passed to \code{\link{as.tex}}.
#' @export
list_to_macros <- function(x, prefix="", collapse = "\n", ...) {
  xnames <- names2(x)
  if (any(xnames == "")) {
    stop("All elements of x must be named.")
  }
  macronames <- str_c(prefix, xnames)
  assert_that(is_tex_command(macronames))

  f <- function(name, val, ...) {
    description <- tex(str_c(as.tex(val, ...), collapse = ""))
    as.character(texcmd("providecommand", name, description))
  }

  tex(str_c(map2_chr(str_c("\\", macronames), unname(x), f, ...),
            collapse = collapse))
}
