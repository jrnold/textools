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
#'    The function \code{texnewcmd} returns an object of class \code{"tex"} with
#'    the LaTeX string.
#' @name texnewcmd
#' @rdname texnewcmd
#' @export
#' @examples
#' texnewcmd("absval",
#'           texcmd("ensuremath", tex("\\lvert#1\\rvert", FALSE)),
#'           nargs = 1)
texnewcmd <- function(name, definition, nargs=0, default=NULL,
                          command = c("providecommand",
                                      "renewcommand",
                                      "newcommand"),
                          starred = FALSE) {
  assert_that(is_tex_command(name))
  assert_that(is_tex_nargs(nargs))
  assert_that(is.flag(starred))
  nargs <- as.integer(nargs)
  command <- match.arg(command)
  if (!is.null(default)) {
    default <- tex(default)
  }
  structure(list(command = command,
                 name = name,
                 definition = tex(definition),
                 nargs = nargs,
                 default = default,
                 starred = starred),
            class = "texnewcmd")
}



# update bracket args using brackets like LaTeX
#' @export
`[.texnewcmd` <- function(x, i, j, ...) {

  i <- as.integer(i)
  x[["nargs"]] <- as.integer(i)
  if (!missing(j) | !is.null(j)) {
    x[["default"]] <- tex(j, ...)
  }
  x
}


render_newcmd_args <- function(nargs, default) {
  if (nargs > 0) {
    nargs_str <- str_c("[", nargs, "]")
    if (is.null(default)) {
      default_str <- ""
    } else {
      default_str <- str_c("[", default, "]")
    }
  } else {
    nargs_str <- ""
    default_str <- ""
  }
  tex(str_c(nargs_str, default_str), escape = FALSE)
}


render_cmd <- function(name, starred) {
  tex(str_c("\\", name, if (starred) "*" else ""))
}


#' @export
format.texnewcmd <- function(x, ...) {
  str_c(render_cmd(x[["command"]], x[["starred"]]),
        "{", str_c("\\", x[["name"]]), "}",
        render_newcmd_args(x[["nargs"]], x[["default"]]),
        "{", x[["definition"]], "}")
}


#' @export
as.character.texnewcmd <- format.texnewcmd


#' @export
as.tex.texnewcmd <- function(x, ...) {
  tex(as.character(x, ...))
}


#' @export
print.texnewcmd <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ": ", as.character(x)))
  invisible(x)
}


#' @export
#' @rdname texnewcmd
texnewcmd_ <- function() {
  mc <- match.call()
  mc[[1L]] <- quote(texnewcmd_)
  tex(eval(mc, parent.frame()), escape = FALSE)
}
formals(texnewcmd_) <- formals(texnewcmd)
