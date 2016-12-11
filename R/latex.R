# Assertation to check for a valid LaTeX macro (command) names
is_tex_command <- function(x) {
  all(str_detect(x, "^[A-Za-z]+[*]?$"))
}
on_failure(is_tex_command) <- function(call, env) {
  str_c(deparse(call$x), " includes invalid LaTeX command names.\n",
        "LaTeX command names can include only letters.")
}


# Tex macro args should be between 1--9
is_tex_nargs <- function(x) {
  assert_that(is.number(x))
  assert_that(x >= 0 && x < 10)
}
on_failure(is_tex_nargs) <- function(call, env) {
  str_c(deparse(call$x),
        " is not a valid value for LaTeX number of arguments.\n",
        "Use an integer 1--9.")
}

# From dplyr
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

#' Key-value argument list
#'
#' Create an argument list of comma separated options and key-value pairs
#' as is often seen in LaTeX commands.
#' For example, "arg1, arg2=value2, arg3, ...".
#'
#' @param x A character vector or list. Elements are converted to tex objects
#'   via \code{\link{as.tex}}.
#' @param ... Additional possibly named arguments
#' @param .escape Whether to LaTeX escape the string values.
#' @return A \code{\link{tex}} object.
#' @export
texkv <- function(x, ..., .escape = FALSE) {
  args <- c(as.list(x), list(...))
  argnames <- names2(args)
  f <- function(k, v) {
    force(.escape)
    v <- as.tex(v, escape = .escape)
    if (k == "") v
    else str_c(k, v, sep = "=")
  }
  # I could do this wit
  tex(str_c(map2_chr(argnames, args, f), collapse = ","))
}


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
  x
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


#' @export
#' @describeIn as.tex This converts a character vector to a \code{tex} object.
#'    Unlike \code{\link{tex}}, it can, and by default, escapes special LaTeX
#'    characters.
#' @param escape Escape LaTeX using the function \code{\link{escape_latex}}.
as.tex.character <- function(x, ..., escape = TRUE) {
  assert_that(is.flag(escape))
  if (escape) {
    x <- escape_latex(x)
  }
  tex(x)
}

#' @export
#' @describeIn as.tex The default method converts \code{x} to a character vector
#'  and then calls \code{as.tex.character}.
as.tex.default <- function(x, ...) {
  as.tex(as.character(x), ...)
}

#' @export
#' @describeIn as.tex This simply returns \code{x}, so it will not escape already
#'   escaped text.
as.tex.tex <- function(x, ...) x
