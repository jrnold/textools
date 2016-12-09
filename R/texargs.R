# Optionally named args
# convert character vector to comma separated list,
# in which unnamed elements are as-is, and named elements
# are key=value pairs.
# given c(k1="x1", "x2") return "k1=x1, x2"
tex_kv <- function(x, ...) {
  x <- c(as.list(x), list(...))
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
#' @param x A list or character object.
#' @param nargs If \code{x} is \code{NULL} or has a length shorter than
#'   \code{nargs}, extra empty arguments are added. If \code{nargs} is less
#'   than the length of \code{x}, then it is ignored. This is to make it easy
#'   to generate empty argument lists list \verb{\{\}\{\}\{\}}.
#' @param escape If \code{TRUE}, then \code{x} will be LaTeX escaped.
#' @param ... Arguments passed to \code{tex}
#'    when converting elements of \code{x}.
#' @export
#' @examples
#' # three arguments
#' texargs(c("a", "b", "c"))
#' # three arguments, with the last two empty
#' texargs(c("a"), nargs = 2)
#' # three empty arguments
#' texargs(nargs = 3)
texargs <- function(x = NULL, ..., nargs = 0L, escape = TRUE) {
  x <- unname(c(as.list(x), list(...)))
  nargs <- max(length(x), nargs)
  # initial empty values
  args <- as.list(rep("", nargs))
  for (i in seq_along(x)) {
    # do it this way instead of map because it is easier to preallocate
    # empty args and assign, than do with map and handle afterwards
    argi <- str_c(as.tex(x[[i]], escape = escape), collapse = "")
    args[[i]] <- str_replace_na(argi, "")
  }
  structure(args, class = "texargs")
}


#' @export
format.texargs <- function(x, ..., trailing = TRUE) {
  # Treat NA's as empty strings
  if (length(x) > 0) {
    ret <- str_c(str_c("{", x, "}"), collapse = "")
  } else {
    ret <- if (trailing) "{}"
      else ""
  }
  tex(ret)
}


`+.texargs` <- function(x, y) {
  assert_that(inherits(y, "texargs"))
  texargs(c(as.list(x), as.list(y)), escape = FALSE)
}


#' @export
as.character.texargs <- function(x, ...) {
  format(x, ...)
}


# LaTeX arguments print as "{arg1}{arg2}{arg3}"
#' @export
as.tex.texargs <- function(x, ...) {
  tex(format(x))
}


#' @export
print.texargs <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
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
#' @param x A list or character vector. It it is a character vector,
#'   it is processed as single optional argument. If it is a list,
#'   each element is converted to
#' @param escape If \code{TRUE}, then the values of x are escaped.
#' @param ... Arguments passed to \code{\link{as.tex}} when
#'   converting elements in \code{x}.
#' @return An object of class \code{"texopts"}.
#' @export
texopts <- function(x = NULL, ..., escape = TRUE) {
  opts <- map(append(list(...), x, 0), as.tex, escape = escape)
  structure(opts, class = "texopts")
}


# print as "[name1=arg1, arg2, ...]"
#' @export
format.texopts <- function(x, brackets = TRUE, ...) {
  if (length(x) > 0) {
    str_c("[", x, "]", collapse = "")
  } else {
    ""
  }
}

# this makes for more LaTeX like writing, even if it isn't
# idiomatic R, which should use [<-
#' @export
`[.texopts` <- function(x, i, j, ..., escape = TRUE) {
  if (is.null(i)) {
    texopts()
  } else {
    x + append(list(...), i, 0L)
  }
}

#' @export
`+.texopts` <- function(x, y) {
  texopts(as.list(x), y)
}

#' @export
as.tex.texopts <- function(x, ...) {
  tex(format(x))
}


#' @export
print.texopts <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}
