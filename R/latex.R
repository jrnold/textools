names2 <- function(x) {
  names(x) %||% rep("", length(x))
}


#' Check that string is a valid TeX command name
#'
#' @param x character vector
#' @return A logical vector indicating which elements elements of the vector
#'   contain valid TeX command names.
#'
#' Without resorting to changing the category codes of characters, valid
#' LaTeX commands can only contain letters.
#'
#' Assertation to check for a valid LaTeX macro (command) names
is_tex_command <- function(x) {
  str_detect(x, "^[A-Za-z]+[*]?$")
}

# is_tex_command <- function(x) {
#   str_detect(x, "^[A-Za-z]+[*]?$")
# }
# on_failure(is_tex_command) <- function(call, env) {
#   str_c(deparse(call$x), " includes invalid LaTeX command names.\n",
#         "LaTeX command names can include only letters.")
# }

#' Convert R object to LaTeX text
#'
#' Marks the given text as (La)TeX, which means functions will
#' know not to perform escaping on it.
#'
#' @param x An R object to be converted to LaTeX.
#' @param ... Arguments passed to methods.
#' @return An object of class \code{c("latex", "character")}. The
#'   \code{"tex"} class is primarily used by other functions in this
#'   package to identify text that is already LaTeX, so it knows not to
#'   escape LaTeX special characters.
#' @seealso \code{\link[utils]{toLatex}}
#' @export
#' @examples
#' tex("Already \\textit{formatted} \\LaTeX text.")
tex <- function(x) {
  structure(x, class = c("tex"))
}

# I'm not sure whether x should be forced to be a string or not.
#' @export
as.character.tex <- function(x, ...) {
  unclass(x)
}

#' @export
print.tex <- function(x, ...) {
  cat(str_c(x, collapse = "\n"))
  invisible(x)
}


#' Convert objects to LaTeX
#'
#' This is the preferred method to convert objects to a
#' \code{tex} object. It is a generic function, so it can be defined for
#' different classes. The default is to convert an object to a character vector
#' and use \code{escape_latex} to escape special LaTeX symbols.
#'
#' @param x The object to convert
#' @param ... Other arguments used by methods
#' @return An object of class \code{"tex"}.
#' @seealso \code{\link{tex}} for a description of code \code{"tex"} objects.
#' @export
as.tex <- function(x, ...) {
  UseMethod("as.tex")
}


#' @describeIn as.tex This converts a character vector to a \code{tex} object.
#'    Unlike \code{\link{tex}}, it can, and by default, escapes special LaTeX
#'    characters.
#' @export
as.tex.default <- function(x, ...) {
  tex(escape_latex(x))
}

#' @export
#' @describeIn as.tex This simply returns \code{x}, so it will not escape already
#'   escaped text.
as.tex.tex <- function(x, ...) x

