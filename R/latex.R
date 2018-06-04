#' Check or make valid LaTeX command name
#'
#' Valid LaTeX command names can only contain letters \code{[A-Za-z]}.
#' The function \code{is_latex_name} checks whether strings are valid LaTeX
#' command names. The function \code{make_tex_name} converts a character vector
#' to valid LaTeX command names by dropping all invalid characters.
#'
#' @param x character vector
#' @param allow_at flag. Allow the character vector to include the \code{@}
#'   character in addition to upper- and lower-cased letters.
#' @return
#' \describe{
#' \item{\code{is_latex_name}}{A logical vector with length \code{length(x)}
#' indicating which elements elements of the vector contain valid TeX
#' command names}
#' \item{}{A character vector with length \code{length(x)} with syntactically
#'   valid LaTeX command names.}
#' }
#' @export
is_latex_name <- function(x, allow_at = FALSE) {
  pattern <- if (allow_at) "^[A-Za-z@]+[*]?$" else "^[A-Za-z]+[*]?$"
  str_detect(x, pattern)
}

#' @export
#' @rdname is_latex_name
make_latex_name <- function(x, allow_at = FALSE) {
  pattern <- if (allow_at[[1]]) "[^A-Za-z@]+" else "[^A-Za-z]+"
  y <- str_replace(x, pattern, "")
  zerolen <- !str_length(y)
  y[zerolen] <- NA_character_
  if (any(zerolen)) {
    warning("Some values could not be converted to valid TeX command names.")
  }
  y
}

#' Convert R object to LaTeX text
#'
#' Marks the given text as LaTeX, which means functions will
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
#' @example examples/ex-LaTeX.R
LaTeX <- function(x) {
  structure(x, class = c("latex"))
}

# I'm not sure whether x should be forced to be a string or not.
#' @export
as.character.latex <- function(x, ...) {
  unclass(x)
}

#' @export
print.latex <- function(x, ...) {
  cat(str_c(x, collapse = "\n"))
  invisible(x)
}

#' Convert objects to LaTeX
#'
#' This is the preferred method to convert objects to a
#' \code{latex} object. It is a generic function, so it can be defined for
#' different classes. The default is to convert an object to a character vector
#' and use \code{escape_latex} to escape special LaTeX symbols.
#'
#' @param x The object to convert
#' @param ... Other arguments used by methods
#' @return An object of class \code{"tex"}.
#' @seealso \code{\link{LaTeX}} for a description of code \code{"latex"} objects.
#' @export
as_latex <- function(x, ...) {
  UseMethod("as_latex")
}

#' @describeIn as_latex This converts a character vector to a \code{LaTeX} object.
#'    Unlike \code{\link{LaTeX}}, it can, and by default, escapes special LaTeX
#'    characters.
#' @export
as_latex.default <- function(x, ...) {
  x <- tryCatch(
    utils::toLatex(x, ...),
    error = function(e) {
      escape_latex(as.character(x), ...)
    }
  )
  LaTeX(x)
}

#' @export
#' @describeIn as_latex Return \code{x}, so as to not valid LaTeX text.
as_latex.latex <- function(x, ...) x

#' @export
#' @describeIn as_latex This returns ""
as_latex.NULL <- function(x, ...) LaTeX("")
