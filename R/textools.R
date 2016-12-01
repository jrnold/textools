#' @import stringr
#' @importFrom purrr map_chr
#' @import assertthat
NULL

stringify <- function(x, ...) {
  str_c(x, ..., sep = "", collapse = "")
}

#' Utility Functions
#'
#' Miscellaneous functions that are useful in latex.
#' \code{brackets}, \code{braces}, and code \code{paren} wraps
#' text in square brackets (\verb{[...]}), curly braces (\verb{\{...\}}),
#' and parentheses (\verb{(...)}), respectively. The \code{math}
#' function wraps text in LaTeX inline math,
#' \verb{$...$}, or display math, \verb{\[...\]}.
#' \code{comment} comments out LaTeX text, \verb{\% ...}.
#'
#' \code{newlines} add line breaks (\verb{\\\\}) to each element in the character vector
#' and concatenates them into a single newline seperated string.
#'
#' @param x character vector
#' @param inline In \code{math}, if \code{TRUE} wraps \code{x} in
#'   display math, e.g., \verb{\[x\]}. Otherwise, the it is returned
#'   as inline math, e.g. \verb{$x$}.
#' @param newline In \code{comment}, append a new-line character to each string.
#' @return A character vector with the transformed text.
#' @name utility-functions
#' @rdname utility-functions
#' @export
parens <- function(x) str_c("(", x, ")")

#' @rdname utility-functions
#' @export
braces <- function(x) str_c("{", x, "}")

#' @rdname utility-functions
#' @export
brackets <- function(x) str_c("[", x, "]")

#' @rdname utility-functions
#' @export
math <- function(x, inline=TRUE) {
  assert_that(is.flag(inline))
  if (inline) {
    str_c("\\(", x, "\\)")
  } else {
    str_c("\\[", x, "\\]")
  }
}

#' @rdname utility-functions
#' @export
newlines <- function(x) {
  # TODO: could add opts for \newline, \\*, \break, \hfill\break, and \linebreak[number]
  # do all of these: https://www.sharelatex.com/learn/Line_breaks_and_blank_spaces
  str_c(x, " \\\\\n", collapse = "")
}

#' @rdname utility-functions
#' @export
imath <- function(x) math(x, inline = TRUE)

#' @rdname utility-functions
#' @export
pctcomment <- function(x, newline = TRUE) {
  assert_that(is.flag(newline))
  nl <- if (newline) "\n" else ""
  str_c("% ", x, nl)
}

# LaTeX Macros can only include letters (but can end in a star)
.valid_macroname <- function(x) {
  str_detect(x, "^[A-Za-z]+[*]?$")
}
on_failure(.valid_macroname) <- function(call, env) {
  str_c(deparse(call$x), " is not a valid LaTeX command name.")
}

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
#' @return An object of class \code{"latex_command"}. This is a \code{list}
#'   with elements \code{cmd}, \code{args}, and \code{optargs}.
#' @export
ltxcmd <- function(command, args=NULL, ..., optargs=NULL) {
  assert_that(is.string(command))
  assert_that(.valid_macroname(command))
  # I don't know if it will handle other cases so enforce it
  assert_that(is.null(optargs) || is.character(optargs))
  if (is.null(optargs)) optargs <- as.character(optargs)
  # if args is NULL then this becomes chr(0)
  args <- unname(append(as.character(args), as.character(list(...))))
  structure(list(command = command, args = args, optargs = optargs),
            class = "latex_command")
}

# Convert optional arguments to string
# named elements become key=value pairs
# multiple arguments are comma separated
# if x is NULL, then it returns NA_character_
.optargs_to_character <- function(x) {
  if (length(x) > 0) {
    if (!is.null(names(x))) {
      x <- ifelse(str_length(names(x)) > 0,
                  str_c(names(x), x, sep = "="),
                  x)
    }
    brackets(str_c(x, collapse = ","))
  } else {
    ""
  }
}

.args_to_character <- function(x, trailing) {
  if (length(x) > 0) {
    str_c(map_chr(x, braces), collapse = "")
  } else {
    if (trailing) {
      "{}"
    } else {
      ""
    }
  }
}

#' @rdname ltxcmd
#' @param trailing If \code{TRUE}, then trailing brackets will be
#'    added to the macro even if there are no arguments.
#' @export
as.character.latex_command <- function(x, ..., trailing=TRUE) {
  assert_that(is.flag(trailing))
  braces_str <- .args_to_character(x[["args"]], trailing)
  brackets_str <- .optargs_to_character(x[["optargs"]])
  str_c("\\", x[["command"]], brackets_str, braces_str)
}

#' @export
format.latex_command <- as.character.latex_command

#' @export
print.latex_command <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ": ", format(x)))
  invisible(x)
}

#' @rdname ltxcmd
#' @export
macro <- function(...) {
  as.character(ltxcmd(...))
}


#' LaTeX Environment
#'
#' This an object represeting a LaTeX environment and which can be rendered
#' to a LaTeX environment.
#'
#' @param name Environment name
#' @param content Text to go in the environment.
#' @param args Environment arguments
#' @param optargs Optional environment arguments (argument inside brackets)
#' @param ... Other arguments needed for method definitions
#' @return An object of class \code{"latex_environment"}, which is a list with
#'   elements \code{name}, \code{content}, \code{args}, and \code{optargs}.
#' @export
ltxenv <- function(name, content, args = NULL, optargs = NULL) {
  assert_that(is.string(name))
  assert_that(.valid_macroname(name))
  assert_that(is.null(optargs) || is.character(optargs))
  if (is.null(optargs)) optargs <- as.character(optargs)
  # process content into a single string
  content <- str_c(content, sep = "", collapse = "\n")
  args <- as.character(args)
  structure(list(name = name,
                 content = content,
                 args = args,
                 optargs = optargs),
                 class = "latex_environment")
}

#' @export
as.character.latex_environment <- function(x, ...) {
  str_c(macro("begin", args = c(x[["name"]], x[["args"]]),
               optargs = x[["optargs"]]),
        x[["content"]],
        macro("end", args = x[["name"]]))
}

#' @export
format.latex_environment <- as.character.latex_environment

#' @export
print.latex_environment <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ":\n", format(x)))
  invisible(x)
}

#' @export
#' @rdname ltxenv
begin <- function(...) as.character(ltxenv(...))


## Latex Command

#' Define a new LaTeX command
#'
#' @param name Command name
#' @param definition Definition of the command
#' @param nargs Number of arguments the command takes
#' @param default Default value for an optional argument
#' @param command Command to use to create the LaTeX command.
#' @param starred If \code{TRUE}, use the starred versions of the
#'   commands, e.g. \verb{\\\\providecommand*}.
#' @return An object of class \code{"latex_newcommand"},
#' @export
ltxnewcommand <- function(name, definition, nargs=0, default=NULL,
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
            class = "latex_newcommand")
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
as.character.latex_newcommand <- function(x, ...) {
  argstr <- .newcommand_arg_strings(x[["nargs"]], x[["default"]])
  str_c("\\", x[["command"]],
        if (x[["starred"]]) "*" else "",
        braces(str_c("\\", x[["name"]])),
        argstr[["nargs"]],
        argstr[["default"]],
        braces(x[["definition"]]))
}

#' @export
format.latex_newcommand <- as.character.latex_newcommand

#' @export
print.latex_newcommand <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ": ", as.character(x)))
  invisible(x)
}

#' @export
#' @rdname ltxnewcommand
providecommand <- function(...) {
  as.character(ltxnewcommand(..., command = "providecommand"))
}

#' @export
#' @rdname ltxnewcommand
newcommand <- function(...) {
  as.character(ltxnewcommand(..., command = "newcommand"))
}

#' @export
#' @rdname ltxnewcommand
renewcommand <- function(...) {
  as.character(ltxnewcommand(..., command = "renewcommand"))
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

## LaTeX Environment

ltxnewenv <- function(name,
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
                 begin_def = as.character(begin_def),
                 end_def = as.character(end_def),
                 num = as.integer(nargs),
                 default = as.character(default),
                 starred = starred),
            class = "latex_newenvironment")
}

#' @export
as.character.latex_newenvironment <- function(x, ...) {
  argstr <- .newcommand_arg_strings(x[["nargs"]], x[["default"]])
  str_c("\\", x[["command"]],
        if (x[["starred"]]) "*" else "",
        braces(str_c("\\", x[["name"]])),
        argstr[["nargs"]],
        argstr[["default"]],
        braces(x[["start_def"]]),
        braces(x[["end_def"]]))
}

#' @export
format.latex_newenvirnoment <- as.character.latex_newenvironment

#' @export
#' @rdname ltxnewenv
newenvironment <- function(...) {
  as.character(ltxnewenv(..., command = "newenvironment"))
}

#' @export
#' @rdname ltxnewenv
renewenvironment <- function(...) {
  as.character(ltxnewenv(..., command = "renewenvironment"))
}

# as.function.latex_newenvironment <- function(x) {
#   function(string, ..., optional = NULL) {
#     args <- .latex_optional_arg(as.character(list(...)),
#                                 optional = optional,
#                                 default = x[["default"]])
#     str_c(.latex_replace_args(begin_def, args),
#           as.character(string),
#           .latex_replace_args(end_def, args))
#   }
# }


# list_to_latex <- function(x, ...) {
#   # LaTeX -> nothing
#   # character -> escaped LaTeX
#   # expression -> verbatim
#   # integer, number -> math
#   # markdown -> rendered to LaTeX
# }

regex_chunker_ <- function(string, regex) {
  idx <- str_locate_all(string, regex)[[1]]
  parsed_str <- vector("list", 2 * nrow(idx) + 2)
  last_idx <- 0L
  k <- 1L
  for (i in seq_len(nrow(idx))) {
    # Get any string before the match
    if ((idx[i, "start"] - 1L) != last_idx) {
      parsed_str[[k]] <-
        list(str_sub(string,
                     last_idx + 1L,
                     idx[i, "start"] - 1L)[[1]],
             FALSE)
      k <- k + 1L
    }
    parsed_str[[k]] <-
      list(str_sub(string, idx[i, "start"], idx[i, "end"])[[1]],
           TRUE)
    k <- k + 1L
    last_idx <- idx[i, "end"]
  }
  if (idx[i, "end"] != nrow(idx)) {
    parsed_str[[k]] <- list(str_sub(string, last_idx + 1L, -1L)[[1]],
                            FALSE)
  }
  compact(parsed_str)
}

regex_chunker <- function(string, regex) {
  map(string, function(x) regex_chunker_(x, regex))
}

chunk_replacer_ <- function(.x,
                            fun_match = identity,
                            fun_nonmatch = identity,
                            collapse = "") {
  FUNC <- function(x) {
    if (x[[2]]) {
      fun_match(x[[1]])
    } else {
      fun_nonmatch(x[[1]])
    }
  }
  newstr <- map_chr(.x, FUNC)
  str_c(newstr, collapse = collapse)
}

chunk_replacer <- function(.x,
                           fun_match = identity,
                           fun_nonmatch = identity,
                           collapse="") {
  map_chr(.x, chunk_replacer_,
          fun_match = fun_match,
          fun_nonmatch = fun_nonmatch,
          collapse = collapse)
}

### To LaTeX

LaTeX <- function(x, escape = FALSE, ...) {
  x <- as.character(x)
  if (escape) {
    x <- escape_latex(x, ...)
  }
  structure(x, class = c("LaTeX", "character"))
}

to_latex <- function(x, ...) {
  UseMethod("to_latex")
}

to_latex.default <- function(x, escape = FALSE, ...) {
  # TODO: call toLatex if it exists
  LaTeX(as.character(x), escape = TRUE, ...)
}

to_latex.LaTeX <- identity

to_latex.Markdown <-

# TODO: automatically set to_latex to use toLatex methods or
# define to_latex = toLatex
to_latex.sessionInfo <- toLatex.sessionInfo
to_latex.citEntry <- toLatex.citEntry

# TODO: use knitr_print() in some way
# TODO: convert Markdown to LaTeX in some way.

list_to_macros <- function(x, to_latex_opts = list(), ...) {
  f <- function(name, val, ...) {
    description <- invoke(to_latex, to_latex_opts, x = val)
    as.character(ltxnewcmd(name, description, ...))
  }
}

URL_REGEX <- str_c(
  # protocol identifier
  "(?:(?:https?|ftp)://)",
  # user:pass authentication
  "(?:\\S+(?::\\S*)?@)?",
  "(?:",
  # IP address exclusion
  # private & local networks
  "(?!(?:10|127)(?:\\.\\d{1,3}){3})",
  "(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})",
  "(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})",
  # IP address dotted notation octets
  # excludes loopback network 0.0.0.0
  # excludes reserved space >= 224.0.0.0
  # excludes network & broacast addresses
  #  (first & last IP address of each class)
  "(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])",
  "(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}",
  "(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))",
  "|",
  # host name
  "(?:(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)",
  # domain name
  "(?:\\.(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)*",
  # TLD identifier
  "(?:\\.(?:[a-z\\u00a1-\\uffff]{2,}))",
  # TLD may end with dot
  "\\.?",
  ")",
  # port number
  "(?::\\d{2,5})?",
  # resource path
  "(?:[/?#]\\S*)?",
  collapse = ""
)

url_escaper <- function(string) {
  replacement <- function(x) {
    str_c("\\url{", x, "}")
  }
  chunk_replacer(regex_chunker(string, URL_REGEX),
                 fun_match = replacement)
}

#' Escape LaTeX special characters
#'
#' Escape special LaTeX special characters in text.
#'
#' The following characters are escaped as follows:
#' \tabular{ll}{
#' Orginal       \tab  Escaped     \cr
#' \verb{\{}     \tab  \verb{\\\{} \cr
#' \verb{\}}     \tab  \verb{\\\}} \cr
#' \verb{#}      \tab  \verb{\\#}  \cr
#' \verb{$}      \tab  \verb{\\$}  \cr
#' \verb{&}      \tab  \verb{\\&}  \cr
#' \verb{_}      \tab  \verb{\\_}  \cr
#' \verb{\%}     \tab  \verb{\\\%} \cr
#' \verb{\\}     \tab  \verb{\\textbackslash{}} \cr
#' \verb{~}      \tab  \verb{\\textasciitilde{}} \cr
#' \verb{^}      \tab  \verb{\\textasciicircum{}}
#' }
#'
#'
#' @param x Character vector
#' @return A character vector with all LaTeX special characters escaped.
#' @export
escape_latex <- function(x) {
  # TODO: escape URLS:
  # e.g. http://stackoverflow.com/questions/26496538/extract-urls-with-regex-into-a-new-data-frame-column
  special_char <- c("{", "}", "#", "$", "&", "_", "%")
  special_char_pattern <-  str_c("[", str_c(special_char, collapse = ""), "]")
  x <- str_replace_all(x, str_c("(", special_char_pattern, ")"), "\\\\\\1")
  # backslashes that are not escaping special characters
  x <- str_replace_all(x, str_c('\\\\', "(?!", special_char_pattern, ")"),
                       '\\\\textbackslash{}')
  x <- str_replace_all(x, fixed("~"), "\\textasciitilde{}")
  x <- str_replace_all(x, fixed("^"), "\\textasciicircum{}")
  x
}
