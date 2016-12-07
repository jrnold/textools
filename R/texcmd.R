# Optionally named args
# convert character vector to comma separated list,
# in which unnamed elements are as-is, and named elements
# are key=value pairs.
# given c(k1="x1", "x2") return "k1=x1, x2"
comma_sep_args <- function(x) {
  optnames <- names(x)
  if (optnames(is.null(optnames))) {
    named_args <- FALSE
  } else {
    named_args <- !is.na(optnames) & str_trim(optnames) != ""
  }
  ret <- as.character(x)
  ret[named_args] <-
    str_c(ret[named_args], optnames[named_args],
          sep = "=", collapse = ", ")
  ret
}


is_tex <- function(x) {
  (inherits(x, "latex") | inherits(x, "texcmd") |
    inherits(x, "texnewcmd") | inherits(x, "texenv") |
     inherits(x, "texnewenv") | inherits(x, "texargs") |
     inherits(x, "texopts"))
}


#' LaTeX arguments to a macro
#'
#' This is just a unnamed character vector with a special
#' \code{latex} method. It also specifies that the elements
#' within it are already valid LaTeX.
#'
#' @param x A list or character object convertible by \code{latex}
#' @param nargs If \code{x} is \code{NULL} or has a length shorter than
#'   \code{nargs}, extra empty arguments are added. If \code{nargs} is less
#'   than the length of \code{x}, then it is ignored. This is to make it easy
#'   to generate empty argument lists list \verb{\{\}\{\}\{\}}.
#' @param escape If \code{TRUE}, then \code{x} will be LaTeX escaped.
#' @param ... Arguments passed to \code{latex}
#' @export
texargs <- function(x, escape = TRUE, nargs = TRUE, ...) {
  assert_that(!is.null(x) & !is.null(nargs))
  if (!is.null(x)) {
    if (is.atomic(x)) {
      args <- latex(x, escape = escape, ...)
    } else if (is.list(x)) {
      args <- map_chr(x, latex, escape = escape, ...)
    }
    if (length(args) < nargs) {
      args <- c(args, rep(character(), nargs - length(args)))
    }
  } else {
    args <- rep(character(), nargs)
  }
  structure(args, class = c("texargs"))
}


format.texargs <- function(x, ...) {
  # Treat NA's as empty strings
  str_c(str_c("{", str_replace_na(x, ""), "}"), collapse = "")
}

# LaTeX arguments print as "{arg1}{arg2}{arg3}"
latex.texargs <- function(x, ...) {
  latex(format(x), escape = FALSE)
}

# Handle case or arguments being NULL or texargs object
render_texargs <- function(x, trailing = TRUE) {
  if (is.null(x[["args"]])) {
    text <- if (trailing) "{}" else ""
  } else {
    text <- format(x)
  }
  latex(text, escape = FALSE)
}


#' LaTeX Option List
#'
#' Object representing LaTeX option lists.
#'
#' @param x An list or character vector.
#' @param escape If \code{TRUE}, then the values of x are
#'   latex escaped.
#' @param ... Arguments passed to \code{\link{latex}}.
#'
#' @return An object of class \code{texopts}.
#' @export
texopts <- function(x, ..., escape = TRUE) {
  assert_that(is.list(x) | is.character(x))
  if (is.list(x)) {
    opts <- map(x, latex, escape = escape, ...)
  } else {
    opts <- latex(x, escape = escape, ...)
  }
  names(opts) <- names(x)
  structure(opts, c("texopts"))
}


# print as "[name1=arg1, arg2, ...]"
format.texopts <- function(x, brackets = TRUE) {
  optstr <- comma_sep_args(x)
  if (brackets) optstr <- str_c("[", optstr, "]")
  optstr
}


latex.texopts <- function(x, ...) {
  latex(format(x), escape = FALSE)
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
#' @param command character Name of the LaTeX command
#' @param args character vector.
#' @param opts character vector containing optional arguments which go in
#'    square brackets. Any named elments are converted to key=value
#'    pairs as is common in many macros. If \code{length(args) > 1},
#'    then arguments are comma separated.
#' @param x In methods, the \code{texcmd} object.
#' @param ... Additional arguments in methods
#' @return \code{texcmd} returns an character vector.
#'   \code{texcmd_} returns an object of class \code{"texcmd"}. This is a \code{list}
#'   with elements \code{cmd}, \code{args}, and \code{opts}.
#' @name texcmd
#' @rdname texcmd
#' @export
#' @examples
#' texcmd("textit", "Italic text")
#' texcmd("includegraphics", optargs=c("width"="8cm"), "path/to/figure.pdf")
#' texcmd("centering")
texcmd <- function(command, args = NULL, opts = NULL) {
  assert_that(is.string(command))
  assert_that(valid_tex_macroname(command))
  # Distinguish NULL (no arguments) from any or empty
  if (!is.null(opts)) {
    opts <- texopts(opts)
  }
  # Distinguish NULL (no arguments) from any or empty args
  if (!is.null(args)) {
    args <- texargs(args)
  }
  structure(list(cmd = command, args = args, opts = opts,
            class = "texcmd"))
}

`[.texcmd` <- function(x, i, j, ...) {
  x[["opts"]] <- texopts(c(list(i, j), list(...)))
  x
}

# format case of options NULL or object texopts
render_texopts <- function(x) {
  if (is.null(x)) {
    text <- ""
  } else {
    text <- format(x)
  }
  latex(text)
}

#' @rdname texcmd
#' @param trailing If \code{TRUE}, then trailing brackets will be
#'    added to the macro even if there are no arguments.
#' @export
format.texcmd <- function(x, ..., trailing=TRUE) {
  assert_that(is.flag(trailing))
  str_c("\\", x[["command"]],
        render_texopts(x[["opts"]]),
        render_texargs(x[["args"]], trailing))
}


#' @export
as.character.texcmd <- format.texcmd


#' @export
texcmd_ <- function() {
  mc <- match.call()
  mc[[1L]] <- quote(texcmd_)
  latex(eval(mc, parent.frame()), escape = FALSE)
}
formals(texcmd_) <- formals(texcmd)


#' @export
#' @rdname texcmd
texcmd_ <- function() {
  # do this computing on language so that texcmd_ and texcmd
  # always have the same args and the args are known for
  # autocompletion
  mc <- match.call()
  mc[[1L]] <- quote(texcmd_)
  latex(eval(mc, parent.frame()), escape = FALSE)
}
formals(texcmd_) <- formals(texcmd)


#' @export
latex.texcmd <- function(x, ...) {
  latex(format(x, ...), escape = FALSE)
}


#' @export
print.texcmd <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ": ", format(x)))
  invisible(x)
}
