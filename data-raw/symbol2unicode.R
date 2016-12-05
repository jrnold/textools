library("readr")

src <- "data-raw/symbol.txt"

symbol2unicode <-
  read_delim(src, "\t", comment = "#",
           col_names = c("unicode", "symbol"))

devtools::use_data(symbol2unicode, overwrite = TRUE)
