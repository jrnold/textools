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
  "floor" = c("lfloor", "rfloor"),
  "ceil" = c("lceil", "rceil"),
  "corner" = c("ulcorner", "urcorner")
)

# For common wrapping
texdelim <- function(text, delim = "{") {
  delimiters <- c("{" = c("{", "}"),
                  "[" = c("[", "]"),
                  "(" = c("(", ")"))
  assert_that(is.character(delim) & length(delim) %in% 0:2)
  if (length(delim) == 1L) {
    delim <- delimiters[delim]
  } else if (length(delim) == 0L) {
    delim <- rep("", 2)
  }
  str_c(delim[1], text, delim[2])
}

"%(%" <- function(x, y) {
  # switch order because "{" %(% "foo" seems more
  # natural than "foo" %(% "{"
  texdelim(y, x)
}

texopt <- function(x) {
  list(x, "[")
}

#' Utility Functions
#'
#' Miscellaneous functions that are useful in latex.
#'
#' The function \code{brackets} wraps text in LaTeX brackets.
#' By default it uses square brackets, \verb{[}, but recognizes
#' multiple types, and will by default auto-size the brackets with
#' \verb{\\left} and \verb{\\right}.
#'
#' The function \code{group} wraps text in curly braces, e.g. \verb{\{x\}},
#' which is a LaTeX group.
#'
#' The \code{math} function wraps text in LaTeX inline math,
#' \verb{$...$}, or display math, \verb{\[...\]}.
#' The function \code{dmath} is a shortcut for display math, i.e.
#' \code{math(x, inline=FALSE)}.
#'
#' \code{texcomment} comments out LaTeX text, \verb{\% ...}.
#'
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
#' brackets("foo")
#' brackets("foo", type = "<")
#' brackets("foo", type = "{")
#' brackets("foo", size = NULL)
#' brackets("foo", size = "\\bigg")
#' group("foo")
#' math("\\frac{1}{2}")
#' math("\\frac{1}{2}", TRUE)
#' imath("\\frac{1}{2}")
#' math("\\frac{1}{2}", dollar = TRUE)
#' texcomment("commented text")
NULL


#' @rdname utility-functions
#' @param type Bracket type
#' @param size Auto-size brackets using \verb{\\left} and \verb{\\right}.
#' @param ... passed to other functions
#' @return character vector
#' @export
brackets <- function(x, type="[", size = "auto") {
  assert_that(is.null(size) || size == "auto" || is.string(size))
  bracks <- BRACKETS[[type]]
  if (is.null(size)) {
    lsize <- rsize <- ""
  } else if (size == "auto") {
    lsize <- "\\left"
    rsize <- "\\right"
  } else {
    lsize <- size[1]
    rsize <- size[2]
  }
  tex(str_c("{", lsize, bracks[1], x, rsize, bracks[2], "}"))
}


#' @rdname utility-functions
#' @export
parens <- partial(brackets, type = "(")


#' @rdname utility-functions
#' @export
group <- function(x) tex(str_c("{", x, "}"))


#' @rdname utility-functions
#' @export
math <- function(x, inline = TRUE, dollar = FALSE) {
  assert_that(is.flag(inline))
  assert_that(is.flag(dollar))
  if (inline) {
    if (dollar) {
      text <- str_c("$", x, "$")
    } else {
      text <- str_c("\\(", x, "\\)")
    }
  } else {
    if (dollar) {
      text <- str_c("$$\n", x, "\n$$")
    } else {
      text <- str_c("\\[\n", x, "\n\\]")
    }
  }
  tex(text)
}


#' @rdname utility-functions
#' @export
newline <- function(x = character()) {
  # nolint start
  # TODO: could add opts for \newline, \\*, \break, \hfill\break, and \linebreak[number]
  # do all of these: https://www.sharelatex.com/learn/Line_breaks_and_blank_spaces
  # nolint end
  tex(str_c(x, "\\\\\n", collapse = ""))
}


#' @rdname utility-functions
#' @export
dmath <- function(x) math(x, inline = FALSE)


#' @rdname utility-functions
#' @export
texcomment <- function(x, newline = TRUE) {
  assert_that(is.flag(newline))
  nl <- if (newline) "\n" else ""
  tex(str_c("% ", x, nl))
}


#' @export
#' @param delim Delimiter to use for \code{verb}.
#'   For example, if \code{delim="|"}, then it produce \verb{\\verb|x|}.
#' @rdname utility-functions
verb <- function(x, delim=c("|", "\"", "!", "=", "#", "^")) {
  # TODO: automatically choose delimiter that doesn't match.
  delim <- match.arg(delim)
  tex(str_c("\\verb", delim, x, delim))
}


#' @export
#' @rdname utility-functions
texrow <- function(x, newline = TRUE) {
  ret <- str_c(tex(x, escape = TRUE), collapse = " & ")
  if (newline) {
    ret <- str_c(ret, newline(), collapse = " ")
  }
  ret
}


# Assertation to check for a valid LaTeX macro (command) names
is_tex_command <- function(x) {
  all(str_detect(x, "^[A-Za-z]+[*]?$"))
}
on_failure(is_tex_command) <- function(call, env) {
  str_c(deparse(call$x), " includes invalid LaTeX command names.\n",
        "LaTeX command names can include only letters.")
}

# Tex macro args should be between 1--9
is_tex_nargs <- function(x) {
  assert_that(is.number(x))
  assert_that(x >= 0 && x < 10)
}
on_failure(is_tex_nargs) <- function(call, env) {
  str_c(deparse(call$x),
        " is not a valid value for LaTeX number of arguments.\n",
        "Use an integer 1--9.")
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
                           collapse = "") {
  map_chr(.x,
          chunk_replacer_,
          fun_match = fun_match,
          fun_nonmatch = fun_nonmatch,
          collapse = collapse)
}
