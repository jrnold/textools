#' Regex for URL validation
#'
#' Regular expression for URL validation from Diego Pierri
#' from this \href{gist}{https://gist.github.com/dperini/729294}.
#'
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


escape_latex_ <- function(x, ellipses=TRUE) {
  # TODO: escape URLS:
  # e.g. http://stackoverflow.com/questions/26496538/extract-urls-with-regex-into-a-new-data-frame-column
  special_char <- c("{", "}", "#", "$", "&", "_", "%")
  special_char_pattern <-  str_c("[", str_c(special_char, collapse = ""), "]")
  x <- str_replace_all(x, str_c("(", special_char_pattern, ")"), "\\\\\\1")
  # backslashes that are not escaping special characters
  x <- str_replace_all(x, str_c('\\\\', "(?!", special_char_pattern, ")"),
                       '\\\\textbackslash{}')
  x <- str_replace_all(x, fixed("~"), "\\textasciitilde{}")
  x <- str_replace_all(x, fixed("^"), "\\textasciicircum{}")
  if (ellipses) {
    # Only replace ellipses when ... exactly
    x <- str_replace_all(x, "(?<!\\.)[.]{3}(?!\\.)", "\\\\dots")
  }
  x
}

# Convert dumb quotes to smart quotes
# Port the Python port https://bitbucket.org/livibetter/smartypants.py/src/461f29bb9ef6da16311f9046bcf448bc36526b98/smartypants.py?at=default&fileviewer=file-view-default
# of the Markdown smartypants
# Only the quotes are relevant. LaTeX already recognizes -- and ---, so don't need
# to convert those, and latex uses ``'' so don't convert those.
smartypants <- function(x, ...) {
}


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
#' \verb{...}    \tab  \verb{\\dots}
#' }
#'
#' @param x Character vector
#' @param url If \code{TRUE}, escape URLs by enclosing them in \verb{\url} macros.
#' @param ellipses If \code{TRUE}, replace \verb{...}.
#' @return A character vector with all LaTeX special characters escaped.
#' @export
escape_latex <- function(x, url=TRUE, ellipses=TRUE) {
  assert_that(is.flag(url))
  assert_that(is.flag(ellipses))
  if (url) {
    chunk_replacer(regex_chunker(x, URL_REGEX),
                   fun_match = function(x) texcmd("url", x),
                   fun_nonmatch = function(x) {
                     escape_latex_(x, ellipses=ellipses)
                   })
  } else {
    escape_latex_(x, ellipses = ellipses)
  }
}
