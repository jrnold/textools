library("stringr")
library("purrr")
regex <- "#[1-9]"
string <- "a #1 b #2 cde#3#45"

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
  if (idx[i, "end"] != nrow(idx)) {
    parsed_str[[k]] <- list(str_sub(string, last_idx + 1L, -1L)[[1]],
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
        fun_match(x[[1]])
    } else {
        fun_nonmatch(x[[1]])
    }
  }
  newstr <- map_chr(.x, FUNC)
  str_c(newstr, collapse = collapse)
}

chunk_replacer <- function(.x,
                           fun_match = identity,
                           fun_nonmatch = identity,
                           collapse="") {
  map_chr(.x, chunk_replacer_,
          fun_match = fun_match,
          fun_nonmatch = fun_nonmatch,
          collapse = collapse)
}

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

url_escaper <- function(string) {
  replacement <- function(x) {
    str_c("\\url{", x, "}")
  }
  chunk_replacer(regex_chunker(string, URL_REGEX),
                 fun_match = replacement)
}








