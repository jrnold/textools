.providecommand <- function(x) {
  tex(sprintf("\\providecommand{\\%s}{%s}", names(x), as_tex(x)))
}


#' Create and Write LaTeX Macro Lists
#'
#' @param
#' @return An object of class \code{latex_macros} which extends
#'  \code{environment}.
#'
#' @export
#' @importFrom rlang as_env names2 dots_splice empty_env as_env
#' @importFrom purrr detect negate
latex_macros <- function(...) {
  x <- dots_splice(...)
  if (any(names2(x) == "")) {
    stop("All arguments must be named", call. = FALSE)
  }
  badnames <- discard(names(x), negate(is_tex_command))
  if (length(badnames) > 0) {
    stop("All names must be valid LaTeX command names. ",
         "The following names are invalid: ",
         str_c("`", badnames, "`"),
         call. = FALSE)
  }
  x <- as_env(x, parent = empty_env())
  class(x) <- c("latex_macros", class(x))
  x
}


#' @export
#' @importFrom glue glue
#' @describeIn latex_macros This checks for the validity of \code{i} before
#'    adding it to the object. If \code{value}
#'    is \code{NULL}, then \code{i} is deleted.
#' @param x An object
#' @param i Index specifying a name.
`[[<-.latex_macros` <- function(x, i, value) {
  if (!is_tex_command(i)) {
    stop(glue("`{i}` is not a valid key.", call. = FALSE))
  }
  if (is.null(value)) {
    rm(list = i, envir = x)
  } else {
    assign(i, as_tex(value), envir = x)
  }
  x
}

#' @export
#' @describeIn Returns a string with the elements of the dictionary formatted as LaTeX commands.
format.latex_macros <- function(x, prefix = "", ...) {
  x <- as.list(x)
  str_c(.providecommand(set_names(x, str_c(prefix, names(x)))),
        collapse = "\n")
}

#' @export
print.latex_macros <- function(x, prefix = "", ...) {
  cat("<latex_macros>\n")
  cat(format(x, prefix = prefix), "\n")
  invisible(x)
}
