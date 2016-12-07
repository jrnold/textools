#' LaTeX Environment
#'
#' This an object represeting a LaTeX environment and which can be rendered
#' to a LaTeX environment.
#'
#' @param name Environment name
#' @param content Text to go in the environment.
#' @param args Environment arguments
#' @param opts Optional environment arguments (argument inside brackets)
#' @param ... Other arguments needed for method definitions
#' @return An object of class \code{"texenv"}, which is a list with
#'   elements \code{name}, \code{content}, \code{args}, and \code{optargs}.
#' @name texenv
#' @rdname texenv
#' @export
#' @examples
#' texenv("figure",
#'        texcmd("includegraphics", "figure.png"),
#'        optargs=c("h"))
#' texenv("center", "Centered text")
#' texenv("enumerate",
#'        latex(paste(c("\\item First item", "\\item Second item"),
#'                      collapse = "\n"),
#'              escape = FALSE))
texenv <- function(name, content, args = NULL, opts = NULL) {
  assert_that(is.string(name))
  assert_that(valid_tex_macroname(name))
  if (!is.null(args)) {
    args <- texargs(args)
  }
  if (!is.null(opts)) {
    opts <- texopts(opts)
  }
  structure(list(name = name, content = latex(content),
                 args = args, opts = opts),
            class = "texenv")
}

# Allow someone to write texenv(x, content, args)[opts]
`[.texenv` <- function(x, i, j, ...) {
  x[["opts"]] <- texopts(c(list(i, j), list(...)))
  x
}


#' @export
format.texenv <- function(x, ...) {
  str_c("\\begin{", x[["name"]], "}",
        render_texopts(x[["opts"]]), render_texargs(x[["args"]]),
        x[["content"]],
        "\\end{", x[["name"]], "}")
}


#' @export
as.character.texenv <- format.texenv


#' @export
latex.texenv <- function(x, ...) {
  latex(format(x), escape = FALSE)
}


#' @export
print.texenv <- function(x, ...) {
  cat(format(x))
  invisible(x)
}


#' @export
#' @rdname texenv
texenv_ <- function() {
  mc <- match.call()
  mc[[1L]] <- quote(texenv_)
  latex(eval(mc, parent.frame()), escape = FALSE)
}
formals(texenv_) <- formals(texenv)
