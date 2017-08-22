.providecommand <- function(x) {
  tex(sprintf("\\providecommand{\\%s}{%s}", names(x), as.tex(x)))
}

#' Convert a list to LaTeX macros
#'
#' For a list or vector. For elements with non-missing names,
#' the elements are converted to a string and t
#'
#' @param x A list or vector
#' @param prefix Prefix added to names
#' @param ... Arguments passed to \code{\link{as.tex}}.
#' @export
write_latex_commands <- function(x, ...) {
  UseMethod("write_latex_commands")
}

#' @rdname write_latex_commands
#' @export
write_latex_commands.default <- function(x, prefix="", ...) {
  if (length(x) == 0) {
    return("")
  }
  xnames <- names2(x)
  if (any(xnames == "")) {
    stop("All elements of x must be named.")
  }
  macronames <- str_c(prefix, xnames)
  assert_that(all(is_tex_command(macronames)))
  .providecommand(set_names(x, macronames))
}


#' Create and Write LaTeX Macro Lists
#'
#' This is an R6 object which simplifies using \code{\link{write_latex_commands}}
#' for writing R objects to LaTeX macros. These objects have simple methods
#' for adding and dropping elements in the list, and to write macros.
#'
#' @export
LaTeXMacroList <- R6::R6Class("LaTeXMacroList", {
  public = list(
    data = rlang::new_environment(),
    prefix = "",
    initialize = function(prefix = "") {
      self$prefix = prefix
    },
    add_all = function(...) {
      env_bind(self$data, UQS(dots_splice(...)))
      invisible(self)
    },
    add = function(value, key, ...) {
      # put value first and return value to make it easier to use in a %>%
      assert_that(is_tex_command(key))
      env_bind(key, as.tex(value, ...))
      invisible(value)
    },
    drop = function(key) {
      self$data[[key]] <- NULL
      invisible(self)
    },
    drop_all = function() {
      self$data <- new
      invisible(self)
    },
    to_macros = function() {
     write_latex_macros(self$data, prefix = self$prefix)
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

coerce_tex_name <- function(x) {
  trailing_star <- str_detect("[*]^", x)
  out <- str_replace(str_to_title(str_replace("[^A-Za-z]", " ")), " +", "")
  str_c(out, if_else(trailing_star, "*", ""))
}
