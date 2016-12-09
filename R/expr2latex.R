library("testthat")

# Symbols converted into LaTeX
# Both Greek letters and others

# Symbols in which the macro name is the same as the expression name
SYMBOLS_ <- c(
  "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
  "gamma", "gamma", "varpi", "phi", "delta", "kappa", "rho",
  "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
  "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
  "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
  "Upsilon", "Omega", "Theta", "Pi", "Phi",
  "cdots", "ldots", "lceil", "rceil",
  "aleph", "nabla")

SYMBOLS <-
  c(
    setNames(paste0("\\", SYMBOLS_), SYMBOLS_),
    # Special names in plotmath for Greek letter variants
    "Upsilon1" = "\\Upsilon",
    "omega1" = "\\varpi",
    "theta1" = "\\vartheta",
    "phi1" = "\\varphi",
    "sigma1" = "\\varsigma",
    # other
    "..." = "\\dots",
    "infinity" = "\\infty",
    "partialdiff" = "\\partial"
  )

symbols_env <- list2env(as.list(SYMBOLS), parent = emptyenv())

# Find all names in a call.
all_names <- function(x) {
  if (is.atomic(x)) {
    character()
  } else if (is.name(x)) {
    as.character(x)
  } else if (is.call(x) || is.pairlist(x)) {
    children <- lapply(x[-1], all_names)
    unique(unlist(children))
  } else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

# Clone an environment
clone_env <- function(env, parent = parent.env(env)) {
  list2env(as.list(env), parent = parent)
}

# Generate latex for unary op, e.g. sin(x)
unary_op <- function(left, right) {
  force(left)
  force(right)
  function(e1) {
    paste0(left, e1, right)
  }
}

# generate latex for binary op, e.g. x + y
binary_op <- function(sep) {
  force(sep)
  function(e1, e2) {
    paste0(e1, sep, e2)
  }
}

# Function environment
f_env <- new.env(parent = emptyenv())

# List of Operators
BINARY_OPS <-
  c("+" = " + ",
    "-" = " - ",
    "/" = " / ",
    # * has special cases to handle
    "%+-%" = " \\pm ",
    "%/%" = " \\div ",
    "%*%" = " \\times ",
    "%.%" = " \\cdot ",
    "^" = "^",
    "[" = "_",
    "==" = " = ",
    "!=" = " \\neq ",
    "<" = " < ",
    "<=" = " \\leq ",
    ">" = " > ",
    ">=" = " \\geq ",
    "%~~%" = " \\approx ",
    "%=~%" = " \\cong ",
    "%==%" = " \\equiv ",
    "%prop%" = " \\propto ",
    "%~%" = " \\sim ",
    "%subset%" = " \\subset ",
    "%subseteq%" = " \\subseteq ",
    "%notsubset%" = " \\not\\subset ",
    "%supset%" = " \\supset ",
    "%supseteq%" = " \\supseteq ",
    "%in%" = " \\in ",
    "%notin%" = " \\notin ",
    "%<->%" = " \\leftrightarrow ",
    "%->%" = " \\rightarrow ",
    "%<-%" = " \\leftarrow ",
    "%up%" = " \\uparrow ",
    "%down" = " \\downarrow ",
    "%<=>%" = " \\Leftrightarrow ",
    "%=>%" = " \\Rightarrow ",
    "%<=%" = " \\Leftarrow ",
    "%dblup%" = " \\Uparrow ",
    "%dbldown%" = " \\Downarrow ",
    # plotmath shows x ~~ y, but ~ appears nowhere else
    "~" = " \\quad "
  )
for (i in names(BINARY_OPS)) {
  f_env[[i]] <- binary_op(BINARY_OPS[i])
}

# Unary Operators
UNARY_OPS <-
  list("{" = list("{\\left{ ", " \\right}}"),
       "(" = list("{\\left( ", " \\right)}"),
       "sqrt" = list("\\sqrt{", "}"),
       "sin" = list("\\sin(", ")"),
       "log" = list("\\log(", ")"),
       "abs" = list("\\left| ", "\\right| "),
       "plain" = list("\\mathrm{", "}"),
       "bold" = list("\\mathbf{", "}"),
       "italic" = list("\\mathbf{\\mathit{", "}}"),
       "hat" = list("\\hat{", "}"),
       "tilde" = list("\\tilde{", "}"),
       "dot" = list("\\dot{", "}"),
       "ring" = list("\\ring{", "}"),
       "bar" = list("\\bar{", "}"),
       "widehat" = list("\\widehat{", "}"),
       "widetilde" = list("\\widetilde{", "}"),
       "displaystyle" = list("\\text{", "}"),
       # These should be adjustable options
       "textstyle" = list("\\normalsize{", "}"),
       "scriptstyle" = list("\\small{", "}"),
       "scriptscriptstyle" = list("\\footnotesize{", "}"),
       "underline" = list("\\underline{", "}"),
       "phantom" = list("\\hphantom{", "}")
      )
for (i in names(UNARY_OPS)) {
  f_env[[i]] <- invoke(unary_op, UNARY_OPS[[i]])
}

# Functions that don't fit into binary or unary

# symbol(x) symbol font?
tex_frac <- function(a, b) {
  paste0("\\frac{", a, "}{", b, "}")
}

sum_like <- function(name) {
  force(name)
  function(x, under, over) {
    paste0("{\\", name, "_{", under, "}^{", over, "} ", x, "}")
  }
}

# generate latex for math operators like log, inf, sup
mathoperator <- function(name) {
  force(name)
  function(x, under = character()) {
    paste0("\\", name, "_{", under, "}")
  }
}

# Other math functions
OTHER_FUNCTIONS <- list(
  "paste" = paste,
  "paste0" = paste0,
  "list" = function(...) {
    paste(..., sep = ", ")
  },
  "frac" = tex_frac,
  "over" = tex_frac,
  "atop" = function(a, b) {
    # Don't use atop in LaTeX
    # See http://www.fi.infn.it/pub/tex/doc/orig/amslatex/amsldoc.pdfa
    paste0("\\genfrac{}{}{0pt}{}{", a, "}{", b, "}")
  },
  "sum" = sum_like("sum"),
  "prod" = sum_like("prod"),
  "integral" = sum_like("int"),
  "union" = sum_like("bigcup"),
  "intersect" = sum_like("bigcap"),
  "lim" = mathoperator("lim"),
  "min" = mathoperator("min"),
  "inf" = mathoperator("inf"),
  "sup" = mathoperator("sup"),
  "group" = function(left, x, right) {
    paste0("{", left, x, right, "}")
  },
  "bgroup" = function(left, x, right) {
    if (left == "") left <- "."
    if (right == "") right <- "."
    paste0("{\\left", left, x, "\\right", right, "}")
  },
  "*" = function(x, y) {
    switch(y,
           # plotmatch handles degree, minute, second differently
           "degree" = paste0(x, "^{\\circ}"),
           "minute" = paste0(x, "'"),
           "second" = paste0(x, "''"),
           paste0(x, " * ", y))
  },
  "symbol" = function(x) {
    stop("symbol() is not supported")
  }
)
for (i in names(OTHER_FUNCTIONS)) {
  f_env[[i]] <- OTHER_FUNCTIONS[[i]]
}

# Extract all calls: functions and apply
all_calls <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    fname <- as.character(x[[1]])
    children <- lapply(x[-1], all_calls)
    unique(c(fname, unlist(children)))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x[-1], all_calls), use.names = FALSE))
  } else {
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

# convert character vector to an environment
chr2env <- function(x, .f = identity,
                    parent = parent.frame(), ...) {
  fx <- lapply(x, .f, ...)
  list2env(setNames(fx, x), parent = parent)
}

# unknown operation / function
unknown_op <- function(op) {
  force(op)
  function(...) {
    contents <- paste(..., collapse = ", ")
    paste0("\\mathrm{", op, "}(", contents, ")")
  }
}

latex_env <- function(expr) {
  # functions/calls appearing in the expression
  call_env <- chr2env(all_calls(expr), .f = unknown_op)

  # Known functions
  f_env <- clone_env(f_env, call_env)

  # symbols appear in the expression
  symbol_env <- chr2env(all_names(expr), parent = f_env)

  # Known symbols
  clone_env(symbols_env, parent = symbol_env)
}

#' Convert Expression to LaTeX
#'
#' Convert an R expression to a LaTeX mathematical equation.
#'
#' Most of the code from this function adapted from the example in
#' \href{http://adv-r.had.co.nz/dsl.html}{R for Data Science},
#' "Domain Specific Languages".
#'
#' @param expr An expression
#' @return A character vector with the LaTeX version of the
#'    expression.
#'
#' @seealso \code{\link{plotmath}}
#' @references http://adv-r.had.co.nz/dsl.html
#' @author Hadley Wickham
#' @export
expr2latex <- function(expr) {
  expr2latex_(substitute(expr))
}


#' @rdname expr2latex
#' @export
expr2latex_ <- function(expr) {
  expr <- as.expression(expr)
  map_chr(expr, ~ eval(.x, latex_env(.x)))
}


#' @export
latex.expression <- function(x, ...) {
  tex(expr2latex(x))
}

#' @export
latex.call <- function(x, ...) {
  tex(as.expression(x))
}

#' @export
latex.name <- function(x, ...) {
  tex(as.expression(x))
}
