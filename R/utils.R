# Make it a single string, dammit.
stringify <- partial(str_c, sep = "", collapse = "")

str_collapse <- function(x, sep = "\n") {
  str_c(x, collapse = sep)
}

# Non-LaTeX string Paris
# TODO extend this with http://stackoverflow.com/questions/13535172/list-of-all-unicodes-open-close-brackets
STR_PAIRS <- c(
  "{" = "}",
  "(" = ")",
  "[" = "]",
  "<" = ">",
  # quotes of various kings
  "``" = "''",
  "`" = "'"
  # TODO: add unicode stuff
)

str_enclose <- function(x, pre=NULL, post=NULL) {
  if (is.null(post) & !is.null(pre)) {
    # try to find a matching closing character
    if (pre %in% names(STR_PAIRS)) {
      post <- STR_PAIRS[pre]
    } else {
      # If no match, double the initial string
      post <- pre
    }
  }
  str_c(pre, x, post)
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
