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
#' @return An object of class \code{"texenv"}, which is a list with
#'   elements \code{name}, \code{content}, \code{args}, and \code{optargs}.
#' @name texenv
#' @rdname texenv
#' @export
texenv_ <- function(name, content, args = NULL, optargs = NULL) {
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
            class = "texenv")
}

#' @export
format.texenv <- function(x, ...) {
  latex(str_c(texcmd("begin", args = c(x[["name"]], x[["args"]]),
              optargs = x[["optargs"]]),
        x[["content"]],
        texcmd("end", args = x[["name"]])), escape=FALSE)
}

#' @export
as.character.texenv <- format.texenv

#' @export
print.texenv <- function(x, ...) {
  cat(str_c("Class ", sQuote(class(x)), ":\n", format(x)))
  invisible(x)
}

#' @export
#' @rdname texenv
texenv <- function(...) format(texenv(...))
