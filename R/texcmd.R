#' LaTeX commands (macros)
#'
#' This returns text wrapped in a LaTeX macro:
#' \verb{\\cmd[optional]\{arg[1]\}\{arg[2]\}}.
#'
#' The function \code{texcmd_} returns a list with the elements
#' in the LaTeX macro, while the function \code{texcmd} returns
#' the rendered LaTeX string as a character vector;
#' executing \code{texcmd(x)} is equivalent to \code{format(texcmd_(x))}.
#'
#' @param command character Name of the LaTeX command
#' @param args character vector.
#' @param optargs character vector containing optional arguments which go in
#'    square brackets. Any named elments are converted to key=value
#'    pairs as is common in many macros. If \code{length(args) > 1},
#'    then arguments are comma separated.
#' @param x In methods, the \code{texcmd} object.
#' @param ... Additional arguments in methods
#' @return \code{texcmd} returns an character vector.
#'   \code{texcmd_} returns an object of class \code{"texcmd"}. This is a \code{list}
#'   with elements \code{cmd}, \code{args}, and \code{optargs}.
#' @name texcmd
#' @rdname texcmd
#' @export
#' @examples
#' texcmd("textit", "Italic text")
#' texcmd("includegraphics", optargs=c("width"="8cm"), "path/to/figure.pdf")
#' texcmd("centering")
texcmd_ <- function(command, args=NULL, ..., optargs=NULL) {
  assert_that(is.string(command))
  assert_that(valid_tex_macroname(command))
  # I don't know if it will handle other cases so enforce it
  assert_that(is.null(optargs) || is.character(optargs))
  if (is.null(optargs)) optargs <- as.character(optargs)
  # if args is NULL then this becomes chr(0)
  args <- unname(c(latex(.x, escape = TRUE),
                   latex(.x, escape = TRUE)))
  structure(list(command = command,
                 args = map(args, ~ latex(.x), escape = TRUE),
                 optargs = optargs),
            class = "texcmd")
}

# Optionally named args
# convert character vector to comma separated list,
# in which unnamed elements are as-is, and named elements
# are key=value pairs.
# given c(k1="x1", "x2") return "k1=x1, x2"
comma_sep_args <- function(x) {
  optnames <- names(x)
  named_args <- optnames != ""
  optargs <- as.character(x)
  optargs[named_args] <-
    str_c(optargs[named_args], optnames[named_args],
          sep = "=", collapse = ", ")
}

latex_arglist <- function(x, escape = TRUE) {
  structure(unname(latex(x, escape = escape)),
            c('latex_arglist', 'character'))
}

# LaTeX arguments print as "{arg1}{arg2}{arg3}"
as.character.latex_arglist <- function(x, trailing = TRUE) {
  str_c(braces(x), collapse = "")
}

latex.latex_arglist <- function(x, ...) {
  latex(as.character(x))
}

latex_optargs <- function(x, escape = FALSE) {
  if (escape) {
    x <- setNames(latex(x, escape = escape), names(x))
  }
  structure(x, c('latex_optargs'))
}

# Latext Optargs print as "[name1=arg1, arg2, ...]"
as.character.latex_optargs(x) {
  str_c("[", comma_sep_args(x), "]")
}

latex.latex_optargs <- function(x, ...) {
  latex(as.character.latex_optargs(x))
}

#' @rdname texcmd
#' @param trailing If \code{TRUE}, then trailing brackets will be
#'    added to the macro even if there are no arguments.
#' @export
format.texcmd <- function(x, ..., trailing=TRUE) {
  assert_that(is.flag(trailing))
  braces_str <- .args_to_character(x[["args"]], trailing)
  brackets_str <- .optargs_to_character(x[["optargs"]])
  str_c("\\", x[["command"]], brackets_str, braces_str)
}

#' @export
as.character.texcmd <- format.texcmd

#' @export
texcmd <- function() {
  mc <- match.call()
  mc[[1L]] <- quote(texcmd_)
  latex(eval(mc, parent.frame()), escape = FALSE)
}
formals(texcmd) <- formals(texcmd_)


#' @export
#' @rdname texcmd
texcmd <- function() {
  # do this computing on language so that texcmd_ and texcmd
  # always have the same args and the args are known for
  # autocompletion
  mc <- match.call()
  mc[[1L]] <- quote(texcmd_)
  latex(eval(mc, parent.frame()), escape = FALSE)
}
formals(texcmd) <- formals(texcmd_)

#' @export
latex.texcmd <- function(x, ...) {
  latex(format(x, ...), escape = FALSE)
}

#' @export
print.texcmd <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ": ", format(x)))
  invisible(x)
}
