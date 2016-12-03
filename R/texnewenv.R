#' LaTeX New Environment
#'
#' Create an object representating a \verb{\\newenvironment}
#' command in LaTeX.
#'
#' When rendered, the object produces the following
#' LaTeX command
#' \verb{\\newenvironment{name}[nargs]default]{begin_def}{end_def}}.
#'
#' @param name LaTeX environment name
#' @param begin_def Beginning definition of the environment
#' @param end_def End definition of the environment
#' @param nargs The number of arguments
#' @param default A default value for the first argument.
#' @param command The LaTeX command to use for defining an environment.
#' @param starred If \code{TRUE}, the use the starred versions of \code{command};
#'   e.g. \verb{\\newenvironment*} and \verb{\\renewenvironment}.
#' @return An object of class \code{latex_newenvironment}.
#'   This is a list with elements \code{name}, \code{begin_def},
#'   \code{nargs}, \code{default}, \code{command}, \code{starred}.
#' @name texnewenv
#' @rdname texnewenv
#' @export
#' @examples
#' # Example environment from
#' # https://www.sharelatex.com/learn/Environments
#' texnewenv("boxed",
#'           latex(paste(c("begin{center}",
#'                         "#1\\\\[1ex]\n",
#'                         "\\begin{tabular}{|p{0.9\\textwidth}|}",
#'                         "\\hline\\\\"), collapse = "\n"),
#'                 FALSE),
#'           latex(paste(c(rep(newline(""), 2),
#'                         texcmd("hline"),
#'                         "\\end{tabular}",
#'                         "\\end{center}"),
#'                       collapse = "\n"),
#'                 FALSE),
#'           nargs = 1)
#'
texnewenv_ <- function(name,
                      begin_def = character(),
                      end_def = character(),
                      nargs = 0,
                      default = NULL,
                      command = c("newenvironment", "renewenvironment"),
                      starred = FALSE) {
  assert_that(is.string(name))
  assert_that(.valid_macroname(name))
  assert_that(is.number(nargs))
  assert_that(nargs >= 0 && nargs < 10)
  nargs <- as.integer(nargs)
  command <- match.arg(command)
  assert_that(is.flag(starred))
  # ensure definition is a single string
  begin_def <- stringify(begin_def) %||% character()
  end_def <- stringify(end_def) %||% character()
  assert_that(is.null(default) || is.character(default))
  structure(list(command = command,
                 name = name,
                 begin_def = as.character(begin_def),
                 end_def = as.character(end_def),
                 nargs = as.integer(nargs),
                 default = as.character(default),
                 starred = starred),
            class = "texnewenv")
}

#' @export
format.texnewenv <- function(x, ...) {
  argstr <- .newcommand_arg_strings(x[["nargs"]], x[["default"]])
  str_c("\\", x[["command"]],
        if (x[["starred"]]) "*" else "",
        braces(x[["name"]]),
        argstr[["nargs"]],
        argstr[["default"]],
        braces(x[["begin_def"]]),
        braces(x[["end_def"]]))
}

#' @export
as.character.texnewenv <- format.texnewenv

#' @export
print.texnewenv <- function(x, ...) {
  xstr <- as.character(x)
  cat("Class ", sQuote(class(x)), ":\n", xstr)
  invisible(x)
}

#' @export
#' @rdname texnewenv
texnewenv <- function() {
  mc <- match.call()
  mc[[1L]] <- quote(texnewenv_)
  latex(eval(mc, parent.frame()), escape = FALSE)
}
formals(texnewenv) <- formals(texnewenv_)
