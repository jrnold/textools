#' LaTeX Arguments
#'
#' Functions and classes to represent and format arguments and
#' lists of arguments for a for a LaTeX command. This has easy options for
#' to wrap text for LaTeX required (\verb{\{\}}) and optional (\verb{[]}) arguments,
#' but is written generally since LaTeX is so flexible that macros can do anything.
#'
#' @param x An object
#' @param kv The argument in \code{x} is concatenated into a list of options and
#'   key-value pairs using \code{\link{texkv}}.
#' @param delim A character vector of length one or two representing
#'  the delimiters used to enclose the argument when printing.
#'  If the element is either \code{"\{"} or \code{"["}, then the
#'  matching closing indicator will be used, otherwise the element will be used
#'  as both the opening and closing delimiters. If two elements are specified,
#'  then the two elements are used as a delimiter. If \code{NULL} or \code{TRUE},
#'  then curly brackets (braces) are used for required arguments. If \code{FALSE}, then
#'  square brackets are used for optional arguments.
#' @return An object of class \code{"texarg"}. This is character vector
#'   with a single element, with attributes \code{"open"} and \code{"close"}
#'   for the opening and closing delimiters for the argument.
#' @export
texarg <- function(x, delim = "{", kv = TRUE) {
  delimiters <- list("{" = c("{", "}"),
                     "[" = c("[", "]"))
  if (inherits(x, "texarg")) {
    return(x)
  }
  assert_that(is.null(delim) |
                (inherits(delim, "character") &&
                length(delim) %in% c(1, 2)) |
                is.flag(delim))
  if (is.null(delim)) {
    delim <- c("{", "}")
  } else if (is.logical(delim)) {
    delim <- if (delim) c("{", "}") else c("[", "]")
  } else if (is.character(delim)) {
    if (length(delim) == 1) {
      delim <- if (delim %in% names(delimiters)) {
        unname(delimiters[[delim]])
      } else {
        rep(delim, 2)
      }
    }
  }
  # Needs to be a single value. Arguments shouldn't be over multiple lines
  if (kv) {
    text <- texkv(x)
  } else {
    text <- str_c(x, collapse = "")
  }
  structure(text, open = delim[1], close = delim[2], class = c("texarg"))
}


#' @export
#' @describeIn texarg Utility function for the common case of optional
#'   LaTeX arguments. It is \code{texarg} with \code{delim = "["}.
texopt <- partial(texarg, delim = "[")


#' @export
as.character.texarg <- function(x, ...) {
  str_c(attr(x, "open"), unclass(x), attr(x, "close"))
}


#' @export
format.texarg <- as.character.texarg

#' @export
print.texarg <- function(x, ...) {
  cat(as.character(x))
  invisible(x)
}

#' @export
as.tex.texarg <- function(x, ...) {
  tex(as.character(x))
}


#' @describeIn texarg \code{texargs} creates a list of \code{texarg} objects.
#'   This is largely used for easy of printing.
#' @param args Either a single argument or a list of arguments
#' @param optargs Optional arguments for a LaTeX command. These are enclosed in
#'   in square brackets and placed before any of the arguments in \code{args}
#'   or \code{...}.
#' @param ... Additional arguments
#' @export
texargs <- function(args = list(), ..., optargs = character(), kv = TRUE) {
  optargs <- map(optargs, texopt, kv = kv)
  all_args <- splice(args, list(...))
  args <- map(map_if(all_args, negate(is.list), function(x) base::list(x)),
              function(l) invoke(texarg, l, kv = kv))
  structure(c(optargs, args), class = c("texargs", "list"))
}

#' @export
as.character.texargs <- function(x, ..., empty = TRUE) {
  if (length(x) == 0) {
    if (empty) "{}" else ""
  } else {
    str_c(map_chr(x, as.character), collapse = "")
  }
}


#' @export
format.texargs <- as.character.texargs


#' @export
as.tex.texargs <- function(x, ...) {
  tex(as.character(x))
}


#' @export
print.texargs <- function(x, ...) {
  print(as.tex(x))
  invisible(x)
}


#' @describeIn texcmd texmacro returns the string for a LaTeX symbol,
#'   i.e. a command without arguments.
#' @param trailing Include trailing curly brackets
#' @param starred Make the macro starred
#' @export
texmacro <- function(name, trailing = FALSE, starred = FALSE) {
  tex(str_c("\\", name,
        if (starred) "*" else "",
        if (trailing) "{}" else ""))
}


#' LaTeX commands (macros)
#'
#' This returns text wrapped in a LaTeX macro:
#' \verb{\\cmd[optional]\{arg[1]\}\{arg[2]\}}.
#'
#' The function \code{texcmd} returns a list with the elements
#' in the LaTeX macro, while the function \code{texcmd_} returns
#' the rendered LaTeX string as a character vector;
#' executing \code{texcmd(x)} is equivalent to \code{format(texcmd_(x))}.
#'
#' @param name Name of the LaTeX command
#' @param ... Command arguments. These are passed to \code{\link{texargs}}.
#'   Elements should be either a character vector, \code{"tex"} object,
#'   \code{"texarg"} object, or a list with arguments for the \code{texargs}
#'   function. By default arguments are assumed to be mandatory. Use the
#'   \code{texopt} function to create optional args.
#' @export
texcmd <- function(name, ...) {
  assert_that(is.string(name))
  ret <- list(name = name, args = texargs(...))
  class(ret) <- "texcmd"
  ret
}


#' @export
as.character.texcmd <- function(x, ..., trailing = TRUE) {
  str_c("\\", x[["name"]], as.character(x[["args"]], empty = trailing))
}

#' @export
print.texcmd <- function(x, ...) {
  cat(as.character(x, ...))
  invisible(x)
}

#' LaTeX Environments
#'
#' Create an object representing text enclosed in a LaTeX environment.
#'
#' @param name Environment name
#' @param body Text content of the macro
#' @param args Arguments to the environment. See \code{\link{texarg}}.
#' @param optargs Optional arguments for the environment. These are arguments
#'   what will be enclosed in square brackets \code{[]}, and preceed arguments in
#'   \code{args}.
#' @param ... Arguments passed to \code{\link{as.tex}} to coerce
#'   \code{body} into a \code{tex} object.
#' @return An object of class \code{"texenv"}, which is a list with
#'  named elements: \code{name} (\code{"character"}),
#'  \code{body} (\code{"character")}), \code{"args"} (\code{"texargs"}).
#' @export
texenv <- function(name, body = "", args = list(), optargs = list(), ...) {
  structure(list(name = name, body = as.tex(body, ...),
                 args = texargs(c(args, optargs = optargs))),
            class = "texenv")
}

#' @export
as.character.texenv <- function(x, ..., trailing = TRUE) {
  str_c(texcmd("begin", x[["name"]]),
        as.character(x[["args"]], empty = trailing), "\n",
        as.character(x[["body"]]), "\n",
        texcmd("end", x[["name"]]), "\n")
}

#' @export
format.texenv <- as.character.texenv


#' @export
print.texenv <- function(x, ...) {
  cat(as.character(x))
  invisible(x)
}
