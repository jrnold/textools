#' Convert a list to LaTeX macros
#'
#' For a list or vector. For elements with non-missing names,
#' the elements are converted to a string and t
#'
#' @param x A list or vector
#' @param prefix Prefix added to names
#' @param collapse If not \code{NULL}, then macros are collapsed to
#'    a single string with \code{collapse} as a seperator.
#' @param ... Arguments passed to \code{\link{as.tex}}.
#' @export
list_to_macros <- function(x, prefix="", collapse = "\n", ...) {
  if (length(x) == 0) {
    return("")
  }
  xnames <- names2(x)
  if (any(xnames == "")) {
    stop("All elements of x must be named.")
  }
  macronames <- str_c(prefix, xnames)
  assert_that(is_tex_command(macronames))

  f <- function(name, val, ...) {
    description <- tex(str_c(as.tex(val, ...), collapse = ""))
    as.character(texcmd("providecommand", name, description))
  }

  tex(str_c(map2_chr(str_c("\\", macronames), unname(x), f, ...),
            collapse = collapse))
}

#' Create and Write LaTeX Macro Lists
#'
#' This is an R6 object which simplifies using \code{\link{list_to_macros}}
#' for writing R objects to LaTeX macros. These objects have simple methods
#' for adding and dropping elements in the list, and to write macros.
#'
#' @export
LaTeXMacroList <- R6::R6Class("LaTeXMacroList", {
  public = list(
    data = list(),
    prefix = "",
    initialize = function(prefix = "") {
      self$prefix = prefix
    },
    add_all = function(`_data`, ...) {
      newdata <- splice(`_data`, list(...))
      for (i in seq_along(`_data`)) {
        self$add(names(newdata)[i], newdata[[i]])
      }
      invisible(self)
    },
    add = function(key, value, ...) {
      self$data[[key]] <- as.tex(value, ...)
      invisible(self)
    },
    drop = function(key) {
      self$data[[key]] <- NULL
      invisible(self)
    },
    drop_all = function(key) {
      self$data <- list()
      invisible(self)
    },
    to_macros = function() {
      list_to_macros(self$data)
    },
    format = function() {
      self$to_macros()
    },
    write = function(file = "", append = FALSE) {
      cat(self$to_macros(), file = file, append = append)
      invisible(self)
    },
    print = function() {
      self$write()
    }
  )
})
