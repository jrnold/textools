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
#' @export
#' @examples
#' texcmd("textit", "Italic text")
#' texcmd("includegraphics", opts=c("width"="8cm"), "path/to/figure.pdf")
#' texcmd("centering")
texcmd <- function(command, args = NULL, opts = NULL) {
  assert_that(is.string(command))
  assert_that(is_tex_command(command))
  # Distinguish NULL (no arguments) from any or empty
  if (!is.null(opts)) {
    opts <- texopts(opts)
  }
  # Distinguish NULL (no arguments) from any or empty args
  if (!is.null(args)) {
    args <- texargs(args)
  }
  structure(list(command = command, args = args, opts = opts),
            class = "texcmd")
}

#' @export
`[.texcmd` <- function(x, ...) {
  x[["opts"]] <- texopts(list(...))
  x
}

# format case of options NULL or object texopts
render_texopts <- function(x) {
  if (is.null(x)) {
    text <- ""
  } else {
    text <- format(x)
  }
  tex(text)
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
#' @rdname texcmd
texcmd_ <- function() {
  # do this computing on language so that texcmd_ and texcmd
  # always have the same args and the args are known for
  # autocompletion
  mc <- match.call()
  mc[[1L]] <- quote(texcmd_)
  tex(eval(mc, parent.frame()), escape = FALSE)
}
formals(texcmd_) <- formals(texcmd)


#' @export
as.tex.texcmd <- function(x, ...) {
  tex(format(x, ...))
}


#' @export
print.texcmd <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}
