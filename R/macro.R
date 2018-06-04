.providecommand <- function(x) {
  str_c("\\providecommand{\\", names(x), "}{", x, "}")
}


#' Create and Write LaTeX Macro Lists
#'
#' @param ... For \code{latex_macros}, named arguments in which the argument
#'   name is the name of LaTeX command the expression is converted to a string
#'   using \code{\link{as_latex}}.  The dots are evaluated using [explicit splicing][dots_list].
#'
#' @return An object of class \code{latex_macros} which extends
#'  \code{environment}.
#'
#' @export
#' @importFrom rlang as_env names2 dots_list empty_env as_env
#' @importFrom purrr detect negate
#' @example examples/ex-macros.R
latex_macros <- function(...) {
  #
  x <- dots_list(...)
  if (length(x) && any(names2(x) == "")) {
    stop("All arguments must be named", call. = FALSE)
  }
  badnames <- discard(names(x), is_latex_name)
  if (length(badnames) > 0) {
    stop("All names must be valid LaTeX command names. ",
         "The following names are invalid: ",
         str_c("`", badnames, "`"),
         call. = FALSE)
  }
  x <- as_env(map(x, as_latex), parent = empty_env())
  class(x) <- c("latex_macros", class(x))
  x
}


#' @export
#' @importFrom glue glue
`[[<-.latex_macros` <- function(x, i, value) {
  if (!is_latex_name(i)) {
    stop(glue("`{i}` is not a valid key.", call. = FALSE))
  }
  if (is.null(value)) {
    rm(list = i, envir = x)
  } else {
    assign(i, as_latex(value), envir = x)
  }
  x
}

#' @export
`$<-.latex_macros` <- function(x, name, value) {
  `[[<-.latex_macros`(x, name, value)
}


#' @export
format.latex_macros <- function(x, prefix = "", ...) {
  x <- as.list(x)
  out <- if (length(x)) {
    str_c(.providecommand(set_names(x, str_c(prefix, names(x)))),
          collapse = "\n")
  } else {
    ""
  }
  LaTeX(out)
}

#' @export
print.latex_macros <- function(x, prefix = "", ...) {
  cat("<latex_macros>\n")
  cat(format(x, prefix = prefix), "\n")
  invisible(x)
}
