#' LaTeX newcommand macro
#'
#' @param name Command name
#' @param definition Definition of the command
#' @param nargs Number of arguments the command takes
#' @param default Default value for an optional argument
#' @param command Command to use to create the LaTeX command.
#' @param starred If \code{TRUE}, use the starred versions of the
#'   commands, e.g. \verb{\\\\providecommand*}.
#' @return An object of class \code{"texnewcmd"},
#' @export
texnewcmd <- function(name, definition, nargs=0, default=NULL,
                          command = c("providecommand",
                                      "renewcommand",
                                      "newcommand"),
                          starred = FALSE) {
  assert_that(is.string(name))
  assert_that(.valid_macroname(name))
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
as.character.texnewcmd <- function(x, ...) {
  argstr <- .newcommand_arg_strings(x[["nargs"]], x[["default"]])
  str_c("\\", x[["command"]],
        if (x[["starred"]]) "*" else "",
        braces(str_c("\\", x[["name"]])),
        argstr[["nargs"]],
        argstr[["default"]],
        braces(x[["definition"]]))
}

#' @export
format.texnewcmd <- as.character.texnewcmd

#' @export
print.texnewcmd <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ": ", as.character(x)))
  invisible(x)
}

#' @export
#' @rdname texnewcmd
providecommand <- function(...) {
  as.character(texnewcmd(..., command = "providecommand"))
}

#' @export
#' @param ... Arguments passed to \code{texnewcmd} in
#'   \code{newcommand} and \code{renewcommand}.
#' @rdname texnewcmd
newcommand <- function(...) {
  as.character(texnewcmd(..., command = "newcommand"))
}

#' @export
#' @rdname texnewcmd
renewcommand <- function(...) {
  as.character(texnewcmd(..., command = "renewcommand"))
}


# TODO: check Rd macro code
# .latex_replace_args <- function(string, args) {
#   for (i in seq_along(args)) {
#     # latex args max out at 9, so don't worry about it
#     string <- str_replace(string, str_c("#", i), args[i])
#   }
#   string
# }

# .latex_optional_arg <- function(args, optional = NULL, default=NULL) {
#   if (!is.null(default)) {
#     if (!is.null(optional)) {
#       args <- c(as.character(optional), args)
#     } else {
#       args <- c(default, args)
#     }
#   }
#   args
# }
#
# as.function.latex_command <- function(x) {
#   function(..., optional=NULL) {
#     args <- .latex_optional_arg(as.character(list(...)),
#                                 optional = optional,
#                                 default = x[["default"]])
#     .latex_replace_args(x[["definition"]], args)
#   }
# }
