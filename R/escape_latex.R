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

#' Regex for URL validation
#'
#' Regular expression for URL validation from Diego Pierri
#' from this \href{gist}{https://gist.github.com/dperini/729294}.
#' This is used by the \code{\link{escape_latex}} function to escape URLs.
#'
#' @seealso \code{\link{escape_latex}}
#' @references
#' \itemize{
#' \item \url{https://gist.github.com/dperini/729294}
#' \item \url{https://mathiasbynens.be/demo/url-regex}
#' }
#' @author Diego Pierri. MIT License.
#' @export
URL_REGEX <- str_c(
  # protocol identifier
  "(?:(?:https?|ftp)://)",
  # user:pass authentication
  "(?:\\S+(?::\\S*)?@)?",
  "(?:",
  # IP address exclusion
  # private & local networks
  "(?!(?:10|127)(?:\\.\\d{1,3}){3})",
  "(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})",
  "(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})",
  # IP address dotted notation octets
  # excludes loopback network 0.0.0.0
  # excludes reserved space >= 224.0.0.0
  # excludes network & broacast addresses
  #  (first & last IP address of each class)
  "(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])",
  "(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}",
  "(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))",
  "|",
  # host name
  "(?:(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)",
  # domain name
  "(?:\\.(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)*",
  # TLD identifier
  "(?:\\.(?:[a-z\\u00a1-\\uffff]{2,}))",
  # TLD may end with dot
  "\\.?",
  ")",
  # port number
  "(?::\\d{2,5})?",
  # resource path
  "(?:[/?#]\\S*)?",
  collapse = ""
)

# http://emailregex.com/
# EMAIL_REGEX <- "(^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$)"

LATEX_SPECIAL_CHARS <- c(
  # backslash
  "\\" = "\\textbackslash{}",
  "{" = "\\{",
  "}" = "\\}",
  "#" = "\\#",
  "$" = "\\$",
  "_" = "\\_",
  "%" = "\\%",
  "&" = "\\&",
  # literal ^
  "^" = "\\textasciicircum{}",
  "~" = "\\textasciitilde{}"
)

escape_latex_ <- function(x) {
  # the main issue is the order to escape {} and \
  # using a replacement function replaces all matches at once and avoids
  # this ordering problem
  str_replace_all(x, "[{}#$_%^~&\\x{5c}]", function(m) LATEX_SPECIAL_CHARS[m])
}

# Convert dumb quotes to smart quotes
# Port the Python port https://bitbucket.org/livibetter/smartypants.py/src/461f29bb9ef6da16311f9046bcf448bc36526b98/smartypants.py?at=default&fileviewer=file-view-default
# of the Markdown smartypants
# Only the quotes are relevant. LaTeX already recognizes -- and ---, so don't need
# to convert those, and latex uses ``'' so don't convert those.
# smartypants <- function(x, ...) {
# }


#' Escape LaTeX special characters
#'
#' Escape special LaTeX special characters in text.
#'
#' The following characters are escaped as follows:
#' \tabular{ll}{
#' Orginal       \tab  Escaped     \cr
#' \verb{\{}     \tab  \verb{\\\{} \cr
#' \verb{\}}     \tab  \verb{\\\}} \cr
#' \verb{#}      \tab  \verb{\\#}  \cr
#' \verb{$}      \tab  \verb{\\$}  \cr
#' \verb{&}      \tab  \verb{\\&}  \cr
#' \verb{_}      \tab  \verb{\\_}  \cr
#' \verb{\%}     \tab  \verb{\\\%} \cr
#' \verb{\\}     \tab  \verb{\\textbackslash{}} \cr
#' \verb{~}      \tab  \verb{\\textasciitilde{}} \cr
#' \verb{^}      \tab  \verb{\\textasciicircum{}} \cr
#' \verb{...}    \tab  \verb{\\dots} \cr
#' \verb{|}      \tab  \verb{\\textbar} \cr
#' \verb{https://cran.r-project.org} \tab
#' \verb{\\url{https://cran.r-project.org}}
#' }
#'
#' @param x Character vector
#' @param url If \code{TRUE}, escape URLs by enclosing them in \verb{\\url} macros
#'     The \verb{\\url} command is from the
#'     \href{https://www.ctan.org/pkg/hyperref}{hyperref}
#'     package.
#' @param ... Other arguments passed to methods
#' @return A character vector with all LaTeX special characters escaped.
#' @export
#' @examples
#' escape_latex(paste(c("These characters will be escaped",
#'                      "{ } # $ & _ % \\ ~ ^ ... | "),
#'                     collapse = ""))
#' escape_latex("By default so are URLs like https://cran.r-project.org")
escape_latex <- function(x, ...) {
  UseMethod("escape_latex")
}

#' @rdname escape_latex
#' @export
escape_latex.character <- function(x, url = TRUE, ...) {
  assert_that(is.flag(url))
  if (url) {
    ret <- chunk_replacer(regex_chunker(x, URL_REGEX),
                   fun_match = function(x) {
                     str_c("\\url{", x, "}")
                   },
                   fun_nonmatch = function(x) {
                     escape_latex_(x)
                   })
  } else {
    ret <- escape_latex_(x)
  }
  LaTeX(ret)
}

#' @rdname escape_latex
#' @export
escape_latex.default <- function(x, ...) {
  escape_latex.character(as.character(x))
}

#' @rdname escape_latex
#' @export
escape_latex.tex <- function(x, ...) x
