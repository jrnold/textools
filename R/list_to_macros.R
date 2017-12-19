.providecommand <- function(x) {
  tex(sprintf("\\providecommand{\\%s}{%s}", names(x), as.tex(x)))
}

#' @rdname write_latex_commands
#' @export


#' Create and Write LaTeX Macro Lists
#'
#' This is an R6 object which simplifies using \code{\link{write_latex_commands}}
#' for writing R objects to LaTeX macros. These objects have simple methods
#' for adding and dropping elements in the list, and to write macros.
#'
#' @export
macrolist <- function(check_keys = TRUE, allow_missing = TRUE) {
  out <- rlang::new_environment()
  attr(out, "check_keys") <- check_keys
  attr(out, "allow_missing") <- allow_missing
  class(out) <- c("macrolist", class(out))
  out
}


#' @export
`[[<-.macrolist` <- function(x, i, value) {
  if (attr("check_keys") && !is_text_command(i)) {
    stop(glue("`{i}` is not a valid key.", call. = FALSE))
  }
  if (attr("allow_missing") & !is.null(value)) {
    stop(glue("value cannot be `NULL`.", call. = FALSE))
  }
  assign(i, as_tex(value), envir = x)
  x
}


#' @export
format.macrolist <- function(x, prefix = "", ...) {
  x <- as.list(x)
  str_c(.providecommand(set_names(x, str_c(prefix, names(x)))),
        collapse = "\n")
}

#' @export
print.macrolist <- function(x, prefix = "", ...) {
  cat(format(x, prefix = prefix))

}

coerce_tex_name <- function(x) {
  trailing_star <- str_detect("[*]^", x)
  out <- str_replace(str_to_title(str_replace("[^A-Za-z]", " ")), " +", "")
  str_c(out, if_else(trailing_star, "*", ""))
}
