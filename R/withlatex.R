#' @export
latex <- list()

# latex CMD
latex[["."]] <- texcmd

# begin
latex[["begin"]] <- texenv

# Escape latex
latex[["X"]] <- as.tex

latex[["x"]] <- tex

# tex arguments
latex[["A"]] <- texarg

latex[["O"]] <- texopt

latex[["%^%"]] <- function(x, y) str_c(x, "^", "{", y, "}")
latex[["%_%"]] <- function(x, y) str_c(x, "_", "{", y, "}")
latex[["%/%"]] <- function(x, y) str_c("\\frac{", x, "}{", y, "}")

latex[["m"]] <- math

latex[["M"]] <- dmath

latex[["C"]] <- texcomment

# Add tex arguments
# latex[["%[%"]] <- function(...) brackets(...)
# latex[["%[[%"]] <- function(...)

latex[["%(%"]] <- brackets

# combine with no-space
latex[["%c%"]] <- function(x, y) tex(str_c(x, y, sep = ""))
# newline
latex[["%n%"]] <- function(x, y) tex(str_c(x, y, sep = "\n"))
# LaTeX line break
latex[["%l%"]] <- function(x, y) tex(str_c(x, y, sep = "\\\\\n"))
# concatenate with space
latex[["% %"]] <- function(x, y) tex(str_c(x, y, sep = " "))

#' @export
with_latex <- function(code) {
  eval(substitute(code), envir = as.list(latex), enclos = parent.frame())
}
