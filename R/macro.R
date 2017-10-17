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
    allow_null = FALSE,
    allow_na = FALSE,
    strings_only = TRUE,
    # function to validate macro input
    # this is useful to catch unexpected bugs in code that could
    # perpetuate to output
    validate = function(x) {
      if (!self$allow_null && is.null(x)) {
        stop("Macro value cannot be null", call. = FALSE)
      } else if (!self$allow_na && any(is.na(x))) {
        stop("NA values are not allowed")
      } else if (strings_only & !is_string(x)) {
        stop("Only character vectors of length one are allowed")
      }
      TRUE
    },
    initialize = function(prefix = "") {
      self$prefix = prefix
    },
    add_all = function(...) {
      args <- dots_splice(...)
      walk2(args, names(args), self$add)
      invisible(self)
    },
    add = function(value, key, ...) {
      # put value first and return value to make it easier to use in a %>%
      assert_that(is_tex_command(key))
      self$data[[key]] <- as.tex(value, ...)
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
    to_latex = function() {
     write_latex_commands(self$data, prefix = self$prefix)
    },
    format = function() {
      self$to_macros()
    },
    write = function(file = "", append = FALSE) {
      cat(self$to_latex(), file = file, append = append)
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
