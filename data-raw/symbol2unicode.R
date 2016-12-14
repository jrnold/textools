library("dplyr")
library("readr")

src <- "data-raw/symbol.txt"

hex2int <- function(x) strtoi(x, 16)
oct2int <- function(x) strtoi(x, 8)
int2hex <- function(x) as.character(as.hexmode(x))
int2oct <- function(x) as.character(as.octmode(x))

# Number of lines in the header chunk
HEADER_LINES <- 62

symbol2unicode <-
  read_delim(src, "\t", skip = 62,
             col_names = c("unicode", "symbol",
                           "unicode_description",
                           "symbol_description")) %>%
  mutate_at(vars(matches("_description")), str_sub, start = 3) %>%
  mutate_at(vars(unicode, symbol), strtoi, base = 16)

# devtools::use_data(symbol2unicode, overwrite = TRUE)


# Other LaTeX data
# Unicode-Math  Mappings
# http://ctan.math.washington.edu/tex-archive/macros/latex/contrib/unicode-math/unicode-math-table.tex
UNICODE_MATH_MAPPINGS <- "http://ctan.math.washington.edu/tex-archive/macros/latex/contrib/unicode-math/unicode-math-table.tex"

PATTERN <- "\\\\UnicodeMathSymbol\\{\"([0-9A-F]+)\\}\\{(.*?)\\}\\{(.*?)\\}\\{(.*?)\\}"

unicode_math <- read_lines(url(UNICODE_MATH_MAPPINGS)) %>%
  str_match(PATTERN) %>%
  as_data_frame() %>%
  setNames(c("text", "unicode", "latex", "group", "description")) %>%
  select(-text) %>%
  mutate(unicode = hex2int(unicode))




