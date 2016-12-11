BRACKETS <- list(
  "(" = c("(", ")"),
  "paren" = c("\\lparen", "\\rparen"),
  "[" = c("\\[", "\\]"),
  "brack" = c("\\lbrack", "\\rbrack"),
  "{" = c("\\{", "\\}"),
  "brace" = c("\\lbrace", "\\rbrace"),
  "floor" = c("\\lfloor", "\\rfloor"),
  "ceil" = c("\\lceil", "\\rceil"),
  "ucorner" = c("\\ulcorner", "\\urcorner"),
  "lcorner" = c("\\llcorner", "\\lrcorner"),
  "moustach" = c("\\lmoustach", "\\rmoustache"),
  "brbrak" = c("\\lbrbrak", "\\rbrbrak"),
  "bag" = c("\\lbag", "\\rbag"),
  "<" = c("\\langle", "\\rangle"),
  "angle" = c("\\langle", "\\rangle"),
  "|" = c("|", "|"),
  "||" = c("\\|", "\\|"),
  "\\" = rep("\\backslash", 2),
  "/" = rep("/", 2),
  "downarrow" = rep("\\downarrow", 2),
  "Downarrow" = rep("\\Downarrow", 2),
  "uparrow" = rep("\\uparrow", 2),
  "Uparrow" = rep("\\Uparrow", 2),
  "updownarrow" = rep("\\Updownarrow", 2),
  "Updownarrow" = rep("\\Updownarrow", 2)
)


left_right <- function(x, cap = FALSE) {
  str_c("\\", if (cap) c("l", "r") else c("L", "R"), x)
}


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
brackets <- function(x, type="[", size = "auto", ...) {
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
  tex(str_c("{", lsize, bracks[1], as.tex(x, ...), rsize, bracks[2], "}"))
}


#' @rdname utility-functions
#' @export
parens <- partial(brackets, type = "(")


#' @rdname utility-functions
#' @export
group <- function(x, ...) {
  tex(str_c("{", as.tex(x, ...), "}"))
}


#' @rdname utility-functions
#' @export
math <- function(x, inline = TRUE, dollar = FALSE, ...) {
  assert_that(is.flag(inline))
  assert_that(is.flag(dollar))
  if (inline) {
    if (dollar) delim <- c("$", "$")
    else delim <- c("\\(", "\\)")
  } else {
    if (dollar) {
      delim <- c("$$\n", "\n$$")
    } else {
      delim <- c("\\[\n", "\n\\]")
    }
  }
  tex(str_c(delim[1], as.tex(x, ...), delim[2]))
}


#' @rdname utility-functions
#' @export
newline <- function(x = character(), ...) {
  # nolint start
  # TODO: could add opts for \newline, \\*, \break, \hfill\break, and \linebreak[number]
  # do all of these: https://www.sharelatex.com/learn/Line_breaks_and_blank_spaces
  # nolint end
  tex(str_c(as.tex(x, ...), "\\\\\n", collapse = ""))
}


#' @rdname utility-functions
#' @export
dmath <- partial(math, inline = FALSE)


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
verb <- function(x, delim="|") {
  # TODO: automatically choose delimiter that doesn't match.
  delim <- match.arg(delim)
  tex(str_c("\\verb", delim, x, delim))
}


#' @export
#' @rdname utility-functions
texrow <- function(x, newline = TRUE, ...) {
  ret <- str_c(as.tex(x, ...), collapse = " & ")
  if (newline) {
    ret <- str_c(ret, newline(), collapse = " ")
  }
  tex(ret)
}
