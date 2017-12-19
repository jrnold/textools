.providecommand <- function(x) {
  tex(sprintf("\\providecommand{\\%s}{%s}", names(x), as_tex(x)))
}


#' Create and Write LaTeX Macro Lists
#'
#' @param ... For \code{latex_macros}, named arguments in which the argument
#'   name is the name of LaTeX command the expression is converted to a string
#'   using \code{\link{as_tex}}.  The dots are evaluated using [explicit splicing][dots_list].
#'
#' @return An object of class \code{latex_macros} which extends
#'  \code{environment}.
#'
#' @export
#' @importFrom rlang as_env names2 dots_list empty_env as_env
#' @importFrom purrr detect negate
#' @example
#' macros <- latex_macros(a = "1", b = "foo")
#' macros
#' # Assign new macros
#' macros$c <- 2 + 2
#' macros$d <- str_c(letters, " ")
#' macros
#' # By default, LaTeX special characters are escaped
#' macros$e <- "$ 2 ^ 3 $"
#' macros$e
#' # use tex() to avoid escaping
#' macros$e <- tex("$ 2 ^ 3 $")
#' macros$e
#' # format() returns a string with LaTeX commands
#' format(macros)
latex_macros <- function(...) {
  #
  x <- dots_list(...)
  if (length(x) && any(names2(x) == "")) {
    stop("All arguments must be named", call. = FALSE)
  }
  badnames <- discard(names(x), is_tex_command)
  if (length(badnames) > 0) {
    stop("All names must be valid LaTeX command names. ",
         "The following names are invalid: ",
         str_c("`", badnames, "`"),
         call. = FALSE)
  }
  x <- as_env(map(x, as_tex), parent = empty_env())
  class(x) <- c("latex_macros", class(x))
  x
}


#' @export
#' @importFrom glue glue
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
  tex(out)
}

#' @export
print.latex_macros <- function(x, prefix = "", ...) {
  cat("<latex_macros>\n")
  cat(format(x, prefix = prefix), "\n")
  invisible(x)
}
