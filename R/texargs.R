# Optionally named args
# convert character vector to comma separated list,
# in which unnamed elements are as-is, and named elements
# are key=value pairs.
# given c(k1="x1", "x2") return "k1=x1, x2"
comma_sep_args <- function(x) {
  xnames <- names(x)
  if (is.null(xnames)) {
    named_args <- FALSE
  } else {
    named_args <- !is.na(xnames) & str_trim(xnames) != ""
  }
  ret <- as.character(x)
  ret[named_args] <-
    str_c(xnames[named_args], ret[named_args], sep = "=")
  str_c(ret, collapse = ", ")
}


#' LaTeX Command Arguments
#'
#' The function \code{textargs} produces an object that represents
#' LaTeX argument lists. The primary purpose of this argument is
#' to easily print to the argument list to the proper LaTeX form
#' and to initialize empty arguments.
#'
#' @param x A list or character object. All elements in \code{x} are
#'   passed to \code{latex} to convert to valid LaTeX.
#' @param nargs If \code{x} is \code{NULL} or has a length shorter than
#'   \code{nargs}, extra empty arguments are added. If \code{nargs} is less
#'   than the length of \code{x}, then it is ignored. This is to make it easy
#'   to generate empty argument lists list \verb{\{\}\{\}\{\}}.
#' @param escape If \code{TRUE}, then \code{x} will be LaTeX escaped.
#' @param ... Arguments passed to \code{latex}
#'    when converting elements of \code{x}.
#' @export
#' @examples
#' # three arguments
#' texargs(c("a", "b", "c"))
#' # three arguments, with the last two empty
#' texargs(c("a"), nargs = 2)
#' # three empty arguments
#' texargs(nargs = 3)
texargs <- function(x, nargs = NULL, escape = TRUE, ...) {
  assert_that(!(is.null(x) && is.null(nargs)))
  assert_that(is.null(nargs) || is_tex_nargs(nargs))
  # TeX args can only be 1--10 in length. But should I check it here?
  assert_that(is.null(x) || (length(x) < 10))
  if (is.null(nargs)) {
    nargs <- length(x)
  } else {
    nargs <- max(length(x), nargs)
  }
  # initial empty values
  args <- rep("", nargs)
  # Args have no names
  x <- unname(x)
  if (!is.null(x)) {
    if (is.atomic(x)) {
      args[seq_along(x)] <- latex(x, escape = escape, ...)
    } else {
      args[seq_along(x)] <- map_chr(x, latex, escape = escape, ...)
    }
  }
  structure(args, class = "texargs")
}


#' @export
format.texargs <- function(x, ...) {
  # Treat NA's as empty strings
  str_c(str_c("{", str_replace_na(x, ""), "}"), collapse = "")
}

#' @export
as.character.texargs <- function(x, ...) {
  format(x, ...)
}


# LaTeX arguments print as "{arg1}{arg2}{arg3}"
#' @export
latex.texargs <- function(x, ...) {
  latex(format(x), escape = FALSE)
}


#' @export
print.texargs <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}


# Handle case or arguments being NULL or texargs object
render_texargs <- function(x, trailing = FALSE) {
  if (is.null(x)) {
    text <- if (trailing) "{}" else ""
  } else {
    text <- format(x)
  }
  latex(text, escape = FALSE)
}


#' LaTeX Option List
#'
#' Create an object representing LaTeX the values in LaTeX
#' optional or default arguments. Arguments appearing in
#' optional or default arguments often take the form of
#' a comma-separated list of either named options, `key=value`,
#' or simply flags, `value`.
#'
#' The primary purpose of this class is to take a vector like
#' \code{c(opta = "val1", "optb", ...)}
#' with optionally named elements and produce a character
#' string like \code{"[opta=val1, optb, ...]"}.
#'
#' @param x A list or character vector.
#' @param escape If \code{TRUE}, then the values of x are
#'   latex escaped.
#' @param ... Arguments passed to \code{\link{latex}} when
#'   converting elements in \code{x}.
#' @return An object of class \code{"texopts"}.
#' @export
texopts <- function(x, ..., escape = TRUE) {
  assert_that(is.list(x) || is.character(x))
  if (is.list(x)) {
    opts <- map(x, latex, escape = escape, ...)
  } else {
    opts <- latex(x, escape = escape, ...)
  }
  names(opts) <- names(x)
  structure(opts, class = "texopts")
}


# print as "[name1=arg1, arg2, ...]"
#' @export
format.texopts <- function(x, brackets = TRUE, ...) {
  optstr <- comma_sep_args(x)
  if (brackets) optstr <- str_c("[", optstr, "]")
  optstr
}


#' @export
latex.texopts <- function(x, ...) {
  latex(format(x), escape = FALSE)
}

#' @export
print.texopts <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}
