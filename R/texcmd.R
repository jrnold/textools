#' LaTeX Macros
#'
#' This returns text wrapped in a LaTeX macro:
#' \verb{\\cmd[optional]\{arg[1]\}\{arg[2]\}}.
#'
#' @param command character Name of the LaTeX command
#' @param args character vector.
#' @param optargs character vector containing optional arguments which go in
#'    square brackets. Any named elments are converted to key=value
#'    pairs as is common in many macros. If \code{length(args) > 1},
#'    then arguments are comma separated.
#' @param x In methods, the \code{macro} object.
#' @param ... Additional arguments in methods
#' @return An object of class \code{"texcmd"}. This is a \code{list}
#'   with elements \code{cmd}, \code{args}, and \code{optargs}.
#' @export
texcmd <- function(command, args=NULL, ..., optargs=NULL) {
  assert_that(is.string(command))
  assert_that(.valid_macroname(command))
  # I don't know if it will handle other cases so enforce it
  assert_that(is.null(optargs) || is.character(optargs))
  if (is.null(optargs)) optargs <- as.character(optargs)
  # if args is NULL then this becomes chr(0)
  args <- unname(append(as.character(args), as.character(list(...))))
  structure(list(command = command, args = args, optargs = optargs),
            class = "texcmd")
}


#' @rdname texcmd
#' @param trailing If \code{TRUE}, then trailing brackets will be
#'    added to the macro even if there are no arguments.
#' @export
as.character.texcmd <- function(x, ..., trailing=TRUE) {
  assert_that(is.flag(trailing))
  braces_str <- .args_to_character(x[["args"]], trailing)
  brackets_str <- .optargs_to_character(x[["optargs"]])
  str_c("\\", x[["command"]], brackets_str, braces_str)
}

#' @export
format.texcmd <- as.character.texcmd

#' @export
print.texcmd <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ": ", format(x)))
  invisible(x)
}

#' @rdname texcmd
#' @export
macro <- function(...) {
  as.character(texcmd(...))
}
