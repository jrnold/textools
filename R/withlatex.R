#' @export
latex <- list()

# latex CMD
latex[["."]] <- texcmd

# begin
latex[["begin"]] <- texenv

# Escape latex
latex[["X"]] <- as.tex

latex[["x"]] <- tex

# tex arguments in [] and {}
latex[["A"]] <- texarg
latex[["O"]] <- texopt

# Overscript, underscript and fractions
latex[["%^%"]] <- function(x, y) str_c(x, "^", "{", y, "}")
latex[["%_%"]] <- function(x, y) str_c(x, "_", "{", y, "}")
latex[["%/%"]] <- function(x, y) str_c("\\frac{", x, "}{", y, "}")

# Quotes
latex[["Q"]] <- function(x) str_c("``", x, "''")
latex[["q"]] <- function(x) str_c("`", x, "'")

# Math
latex[["m"]] <- math
latex[["M"]] <- dmath

# Comments
latex[["C"]] <- texcomment

# Add tex arguments
# latex[["%[%"]] <- function(...) brackets(...)
# latex[["%[[%"]] <- function(...)

# Parentheses. I should make it so {...} %)% ")" and "(" %(% {...} both work.
latex[["%(%"]] <- brackets

# Newline and continuations
# combine with no-space
latex[["%c%"]] <- function(x, y) tex(str_c(x, y, sep = ""))
# newline
latex[["%n%"]] <- function(x, y) tex(str_c(x, y, sep = "\n"))
# LaTeX line break
latex[["%l%"]] <- function(x, y) tex(str_c(x, y, sep = "\\\\\n"))
# concatenate with space
latex[["% %"]] <- function(x, y) tex(str_c(x, y, sep = " "))
latex[["%~%"]] <- function(x, y) tex(str_c(x, y, sep = "~"))

#' @export
with_latex <- function(code) {
  eval(substitute(code), envir = as.list(latex), enclos = parent.frame())
}
