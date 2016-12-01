#' @import stringr
#' @import purrr
#' @import assertthat
NULL

stringify <- function(x, ...) {
  str_c(x, ..., sep = "", collapse = "")
}

#' Utility Functions
#'
#' Miscellaneous functions that are useful in latex.
#' \code{brackets}, \code{braces}, and code \code{paren} wraps
#' text in square brackets (\verb{[...]}), curly braces (\verb{\{...\}}),
#' and parentheses (\verb{(...)}), respectively. The \code{math}
#' function wraps text in LaTeX inline math,
#' \verb{$...$}, or display math, \verb{\[...\]}.
#' \code{comment} comments out LaTeX text, \verb{\% ...}.
#'
#' \code{newlines} add line breaks (\verb{\\\\}) to each element in the character vector
#' and concatenates them into a single newline seperated string.
#'
#' @param x character vector
#' @param inline In \code{math}, if \code{TRUE} wraps \code{x} in
#'   display math, e.g., \verb{\[x\]}. Otherwise, the it is returned
#'   as inline math, e.g. \verb{$x$}.
#' @param newline In \code{comment}, append a new-line character to each string.
#' @return A character vector with the transformed text.
#' @name utility-functions
#' @rdname utility-functions
#' @export
parens <- function(x) str_c("(", x, ")")

#' @rdname utility-functions
#' @export
braces <- function(x) str_c("{", x, "}")

#' @rdname utility-functions
#' @export
brackets <- function(x) str_c("[", x, "]")

#' @rdname utility-functions
#' @export
math <- function(x, inline=TRUE) {
  assert_that(is.flag(inline))
  if (inline) {
    str_c("\\(", x, "\\)")
  } else {
    str_c("\\[", x, "\\]")
  }
}

#' @rdname utility-functions
#' @export
newlines <- function(x) {
  # TODO: could add opts for \newline, \\*, \break, \hfill\break, and \linebreak[number]
  # do all of these: https://www.sharelatex.com/learn/Line_breaks_and_blank_spaces
  str_c(x, " \\\\\n", collapse = "")
}

#' @rdname utility-functions
#' @export
imath <- function(x) math(x, inline = TRUE)

#' @rdname utility-functions
#' @export
pctcomment <- function(x, newline = TRUE) {
  assert_that(is.flag(newline))
  nl <- if (newline) "\n" else ""
  str_c("% ", x, nl)
}

# LaTeX Macros can only include letters (but can end in a star)
.valid_macroname <- function(x) {
  str_detect(x, "^[A-Za-z]+[*]?$")
}
on_failure(.valid_macroname) <- function(call, env) {
  str_c(deparse(call$x), " is not a valid LaTeX command name.")
}

# Convert optional arguments to string
# named elements become key=value pairs
# multiple arguments are comma separated
# if x is NULL, then it returns NA_character_
.optargs_to_character <- function(x) {
  if (length(x) > 0) {
    if (!is.null(names(x))) {
      x <- ifelse(str_length(names(x)) > 0,
                  str_c(names(x), x, sep = "="),
                  x)
    }
    brackets(str_c(x, collapse = ","))
  } else {
    ""
  }
}

.args_to_character <- function(x, trailing) {
  if (length(x) > 0) {
    str_c(map_chr(x, braces), collapse = "")
  } else {
    if (trailing) {
      "{}"
    } else {
      ""
    }
  }
}

## LaTeX Environment
regex_chunker_ <- function(string, regex) {
  idx <- str_locate_all(string, regex)[[1]]
  parsed_str <- vector("list", 2 * nrow(idx) + 2)
  last_idx <- 0L
  k <- 1L
  for (i in seq_len(nrow(idx))) {
    # Get any string before the match
    if ((idx[i, "start"] - 1L) != last_idx) {
      parsed_str[[k]] <-
        list(str_sub(string,
                     last_idx + 1L,
                     idx[i, "start"] - 1L)[[1]],
             FALSE)
      k <- k + 1L
    }
    parsed_str[[k]] <-
      list(str_sub(string, idx[i, "start"], idx[i, "end"])[[1]],
           TRUE)
    k <- k + 1L
    last_idx <- idx[i, "end"]
  }
  if (last_idx == 0 || last_idx != nrow(idx)) {
    parsed_str[[k]] <-
      list(str_sub(string, last_idx + 1L, -1L)[[1]],
           FALSE)
  }
  compact(parsed_str)
}

regex_chunker <- function(string, regex) {
  map(string, function(x) regex_chunker_(x, regex))
}

chunk_replacer_ <- function(.x,
                            fun_match = identity,
                            fun_nonmatch = identity,
                            collapse = "") {
  FUNC <- function(x) {
    if (x[[2]]) {
      ret <- fun_match(x[[1]])
    } else {
      ret <- fun_nonmatch(x[[1]])
    }
    ret
  }
  newstr <- map_chr(.x, FUNC)
  str_c(newstr, collapse = collapse)
}

chunk_replacer <- function(.x,
                           fun_match = identity,
                           fun_nonmatch = identity,
                           collapse="") {
  map_chr(.x,
          chunk_replacer_,
          fun_match = fun_match,
          fun_nonmatch = fun_nonmatch,
          collapse = collapse)
}

