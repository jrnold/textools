#' Convert decimal integer to arbitrary base
#'
#' Convert a non-negative decimal integer to an arbitrary base with
#' arbitrary symbols.
#'
#' @param x Vector of non-negative integers to convert
#' @param base Either a positive integer specifying the base number or
#'  a character string of the symbols to use for the base. If \code{base}
#'  is a number, then the default symbols are: 0, ..., 9, A, ..., Z.
#' @param drop_leading If \code{TRUE}, for positive numbers, any leading zeros are dropped.
#' @param width Minimum width of the strings,
#'   they will be padded with zeros to at least that length.
#' @return A character vector of the integers in the new base
#' @export
dec2base <- function(x, base, drop_leading = TRUE, width = 1L) {
  default_symbols <- c(0:9, LETTERS)
  assert_that( (is.number(base) && base > 0) ||
                 (is.character(base) && length(base) > 0))
  assert_that(is.flag(drop_leading))
  if (is.numeric(base)) {
    base <- as.integer(base)
    symbols <- default_symbols
    if (base > length(symbols)) {
      stop(str_c("For values of base > ", length(symbols),
                 " you need to provide a character vector with symbols"))
    }
  } else {
    # If a single string "0123", then split into c("0", "1", "2", "3"))
    if (length(base) == 1 && str_length(base) > 1) {
      base <- str_split(base, "")[[1]]
    }
    # there shouldn't be any duplicates, better to have an error
    # than go on in ignorance
    assert_that(!any(duplicated(base)))
    symbols <- base
    base <- length(base)
  }
  # string length
  maxpower <- trunc(log(max(x, 1), base = base)) + 1L
  powers <- base ^ ((maxpower - 1):0)
  xstr <- map_chr(x, function(x) {
    digits <- floor( (x %% (base * powers) / powers))
    if (drop_leading) {
      if (sum(digits) == 0) {
        # exact zero
        digits <- 0L
      } else {
        # remove any leading zeros if > 0
        digits <- digits[min(which(digits > 0)):length(digits)]
      }
    }
    str_c(symbols[digits + 1L], collapse = "")
  })
  if (width > 1) {
    xstr <- str_pad(xstr, width, symbols[1], side = "left")
  }
  xstr
}

#' @describeIn dec2base Since LaTeX doesn't support numbers in command names,
#'  encode numbers in base 26 with letters A-Z, or base 52 using both upper and
#'  lowercase letters. Note that this means that A=0, B=1, ... .
#'  An alternative is to use Roman numerals instead; see \code{\link[utils]{as.roman}}.
#' @param lower If \code{TRUE}, use lower case letters as well as upper case letters.
#' @param ... Arguments passed to \code{dec2base}.
#' @export
dec2alpha <- function(x, lower = FALSE, ...) {
  if (lower) {
    base <- c(base::LETTERS, base::letters)
  } else {
    base <- base::LETTERS
  }
  dec2base(x, base, ...)
}
