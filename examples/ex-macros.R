macros <- latex_macros(a = "1", b = "foo")
macros
# Assign new macros
macros$c <- 2 + 2
macros$d <- paste(letters, collapse = " ")
macros
# By default, LaTeX special characters are escaped
macros$e <- "$ 2 ^ 3 $"
macros$e
# use tex() to avoid escaping
macros$e <- tex("$ 2 ^ 3 $")
macros$e
# format() returns a string with LaTeX commands
format(macros)
