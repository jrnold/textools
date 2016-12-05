stringify <- function(x, ...) {
  str_c(x, ..., sep = "", collapse = "")
}

BRACKETS <- list(
  "[" = c("\\[", "\\]"),
  "(" = c("(", ")"),
  "{" = c("\\{", "\\}"),
  "|" = c("|", "|"),
  "||" = c("\\|", "\\|"),
  "<" = c("langle", "rangle"),
  "floor" = c("lfloor", "rceil"),
  "ceil" = c("lceil", "rceil"),
  "corner" = c("ulcorner", "urcorner")
)


#' Utility Functions
#'
#' Miscellaneous functions that are useful in latex.
#' \code{brackets}, \code{braces}, and code \code{paren} wraps
#' text in square brackets (\verb{[...]}), curly braces (\verb{\{...\}}),
#' and parentheses (\verb{(...)}), respectively. The \code{math}
#' function wraps text in LaTeX inline math,
#' \verb{$...$}, or display math, \verb{\[...\]}.
#' \code{texcomment} comments out LaTeX text, \verb{\% ...}.
#' \code{texrow} concates a character string with ampersands,
#' \verb{x1 & x2 & x3 \\\\}.
#'
#' \code{newlines} add line breaks (\verb{\\\\}) to each element in the character vector
#' and concatenates them into a single newline seperated string.
#'
#' @param x character vector
#' @param inline In \code{math}, if \code{TRUE} wraps \code{x} in
#'   display math, e.g., \verb{\[x\]}. Otherwise, the it is returned
#'   as inline math, e.g. \verb{\(x\)}.
#' @param newline In \code{texcomment}, append a new-line character to each string.
#' @param dollar In \code{math}, use \verb{$$} for display math,
#'    and \verb{$} for inline math, instead of the defaults.
#' @return A character vector with the transformed text.
#' @name utility-functions
#' @rdname utility-functions
#' @examples
#' parens("foo")
#' braces("foo")
#' brackets("foo")
#' math("\\frac{1}{2}")
#' math("\\frac{1}{2}", TRUE)
#' imath("\\frac{1}{2}")
#' math("\\frac{1}{2}", dollar = TRUE)
#' texcomment("commented text")
NULL

BRACKET_SIZES <- c("\\big", "\\Big", "\\bigg", "\Bigg")

FONT_SIZES <- c("\\tiny", "\\scriptsize", "\\footnotesize",
                "\\small", "\\normalsize", "\\large",
                "\\Large", "\\LARGE", "\\huge", "\\Huge")

FONT <-
  c(families =
      list("rm" = list(command = "\\textrm", switch = "\\rmfamily",
                       description = "serif (Roman)"),
           "sf" = list(command = "\\textsf", switch = "\\sffamily",
                       description = "sans serif"),
           "tt" = list(command = "\\texttt", switch = "\\ttfamily",
                       description = "typewriter (monospace)")),
    styles =
      list(md = list(command = "\\textmd", switch = "\\mdseries",
                     description = "medium"),




#' @rdname utility-functions
#' @param type Bracket type
#' @param size Auto-size brackets using \verb{\\left} and \verb{\\right}.
#' @param ... passed to other functions
#' @export
brackets <- function(x, type="[", size = "auto") {
  bracks <- BRACKETS[type]
  if (size == "auto") {
    lsize <- "\\left"
    rsize <- "\\right"
  } else {
    lsize <- size[1]
    rsize <- size[2]
  }
  latex(str_c("{", lsize, bracks[1], latex(x, TRUE), rsize, bracks[2], "}"))
}


#' @rdname utility-functions
#' @export
parens <- partial(brackets, type = "(")


#' @rdname utility-functions
#' @export
braces <- function(x) str_c("{", x, "}")


#' @rdname utility-functions
#' @export
math <- function(x, inline = TRUE, dollar = FALSE) {
  assert_that(is.flag(inline))
  assert_that(is.flag(dollar))
  if (inline) {
    if (dollar) {
      str_c("$", x, "$")
    } else {
      str_c("\\(", x, "\\)")
    }
  } else {
    if (dollar) {
      str_c("$$\n", x, "\n$$")
    } else {
      str_c("\\[\n", x, "\n\\]")
    }
  }
}


#' @rdname utility-functions
#' @export
newline <- function(x = character()) {
  # nolint start
  # TODO: could add opts for \newline, \\*, \break, \hfill\break, and \linebreak[number]
  # do all of these: https://www.sharelatex.com/learn/Line_breaks_and_blank_spaces
  # nolint end
  str_c(x, "\\\\\n", collapse = "")
}

#' @rdname utility-functions
#' @export
imath <- function(x) math(x, inline = TRUE)

#' @rdname utility-functions
#' @export
texcomment <- function(x, newline = TRUE) {
  assert_that(is.flag(newline))
  nl <- if (newline) "\n" else ""
  str_c("% ", x, nl)
}

#' @export
#' @param delim Delimiter to use for \code{verb}.
#'   For example, if \code{delim="|"}, then it produce \verb{\\verb|x|}.
#' @rdname utility-functions
verb <- function(x, delim=c("|", "\"", "!", "=", "#", "^")) {
  # TODO: automatically choose delimiter that doesn't match.
  delim <- match.arg(delim)
  str_c("\\verb", delim, x, delim)
}

#' @export
#' @rdname utility-functions
texrow <- function(x, newline = TRUE) {
  ret <- str_c(latex(x, escape = TRUE), collapse = " & ")
  if (newline) {
    ret <- str_c(ret, newline(), collapse = " ")
  }
  ret
}

# Assertation to check for a valid LaTeX macro (command) names
valid_tex_macroname <- function(x) {
  all(str_detect(x, "^[A-Za-z]+[*]?$"))
}
on_failure(valid_tex_macroname) <- function(call, env) {
  str_c(deparse(call$x), " includes invalid LaTeX command names.")
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

# splits a string into chunks: with matches and nonmatches
# of a regex
regex_chunker_ <- function(string, regex) {
  idx <- str_locate_all(string, regex)[[1]]
  parsed_str <- vector("list", 2 * nrow(idx) + 2)
  last_idx <- 0L
  k <- 1L
  for (i in seq_len(nrow(idx))) {
    # Get any string before the match
    if ( (idx[i, "start"] - 1L) != last_idx) {
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

# splits each string in a character vector into a list
# with matches (chunks) and non-matches of a character vector
regex_chunker <- function(string, regex) {
  map(string, function(x) regex_chunker_(x, regex))
}

# For a list produced by regex_chunker,
# apply functions to the matched and non-matched elements
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

# For a list of lists produced by regex_chunker()
# apply functions to the matched and non-matched functions
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
