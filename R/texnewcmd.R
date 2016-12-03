#' LaTeX newcommand macro
#'
#' @param name Command name
#' @param definition Definition of the command
#' @param nargs Number of arguments the command takes
#' @param default Default value for an optional argument
#' @param command Command to use to create the LaTeX command.
#' @param starred If \code{TRUE}, use the starred versions of the
#'   commands, e.g. \verb{\\\\providecommand*}.
#' @return The function \code{texnewcmd_} returns an object of class \code{"texnewcmd"},
#'    while is a list with elements \code{name}, \code{definition}, \code{nargs},
#'    \code{default}, \code{command}, and \code{starred}.
#'    The function \code{texnewcmd} returns an object of class \code{"latex"} with
#'    the LaTeX string.
#' @name texnewcmd
#' @rdname texnewcmd
#' @export
#' @examples
#' texnewcmd("absval",
#'           texcmd("ensuremath", latex("\\lvert#1\\rvert", FALSE)),
#'           nargs = 1)
texnewcmd_ <- function(name, definition, nargs=0, default=NULL,
                          command = c("providecommand",
                                      "renewcommand",
                                      "newcommand"),
                          starred = FALSE) {
  assert_that(is.string(name))
  assert_that(valid_tex_macroname(name))
  assert_that(is.number(nargs))
  assert_that(nargs >= 0 && nargs < 10)
  nargs <- as.integer(nargs)
  command <- match.arg(command)
  assert_that(is.flag(starred))
  # ensure definition is a single string
  definition <- stringify(definition)
  assert_that(is.null(default) || is.character(default))
  structure(list(command = command,
                 name = name, definition = definition,
                 nargs = nargs,
                 default = default,
                 starred = starred),
            class = "texnewcmd")
}

.newcommand_arg_strings <- function(nargs, default) {
  if (nargs > 0) {
    nargs_str <- brackets(nargs)
    if (length(default) > 0) {
      default_str <- .optargs_to_character(default)
    } else {
      default_str <- ""
    }
  } else {
    nargs_str <- ""
    # If no arguments, then ignore defaults
    default_str <- ""
  }
  list(nargs = nargs_str, default = default_str)
}

#' @export
format.texnewcmd <- function(x, ...) {
  argstr <- .newcommand_arg_strings(x[["nargs"]], x[["default"]])
  str_c("\\", x[["command"]],
        if (x[["starred"]]) "*" else "",
        braces(str_c("\\", x[["name"]])),
        argstr[["nargs"]],
        argstr[["default"]],
        braces(x[["definition"]]))
}

#' @export
as.character.texnewcmd <- format.texnewcmd

#' @export
latex.texnewcmd <- function(x, ...) {
  latex(as.character(x, ...), escape = FALSE)
}

#' @export
print.texnewcmd <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ": ", as.character(x)))
  invisible(x)
}

#' @export
#' @rdname texnewcmd
texnewcmd <- function() {
  mc <- match.call()
  mc[[1L]] <- quote(texnewcmd_)
  latex(eval(mc, parent.frame()), escape = FALSE)
}
formals(texnewcmd) <- formals(texnewcmd_)
