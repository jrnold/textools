library("testthat")

to_math <- function(x) {
  expr <- substitute(x)
  eval(expr, latex_env(expr))
}

greek <- c(
  "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
  "gamma", "gamma", "varpi", "phi", "delta", "kappa", "rho",
  "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
  "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
  "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
  "Upsilon", "Omega", "Theta", "Pi", "Phi")

symbols <- c("cdots", "ldots", "lceil", "rceil",
             "aleph", "nabla")

greek_list <- setNames(paste0("\\", greek), greek)

SYMBOLS <-
  c(
    # Special names in plotmath
    # for variants
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


greek_env <- list2env(as.list(greek_list), parent = emptyenv())

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

clone_env <- function(env, parent = parent.env(env)) {
  list2env(as.list(env), parent = parent)
}


unary_op <- function(left, right) {
  force(left)
  force(right)
  function(e1) {
    paste0(left, e1, right)
  }
}

binary_op <- function(sep) {
  force(sep)
  function(e1, e2) {
    paste0(e1, sep, e2)
  }
}

f_env <- new.env(parent = emptyenv())
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

mathoperator <- function(name) {
  force(name)
  function(x, under = character()) {
    paste0("\\", name, "_{", under, "}")
  }
}

# Other math functions
FUNCTIONS = list(
  "paste" = paste,
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
    # Options to support it include translating all
    # Symbol fonts to latex equivalents
    # Or
    stop("symbol() is not supported")
    # \includepackage{psnfsse}
    paste0("\\Pisymbol{", x, "}")
  }
)
for (i in names(FUNCTIONS)) {
  f_env[[i]] <- FUNCTIONS[[i]]
}

# Labelling

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

chr2env <- function(x, .f = identity,
                    ...) {
  args <- list(...)
  env_argnames <- c("parent", "hash", "size")
  env_args <- keep(args, ~ .x %in% env_argnames)
  print(env_args)
  f_args <-
    c(list(X = x, FUN = .f),
      discard(args, ~ .x %in% env_argnames))
  fx <- invoke(lapply, f_args)
  invoke(list2env, c(list(setNames(fx, x)), env_args))
}

unknown_op <- function(op) {
  force(op)
  function(...) {
    contents <- paste(..., collapse = ", ")
    paste0("\\mathrm{", op, "}(", contents, ")")
  }
}

latex_env <- function(expr) {
  calls <- all_calls(expr)
  call_env <- chr2env(calls, .f = unknown_op)

  # Known functions
  f_env <- clone_env(f_env, call_env)

  # Default symbols
  symbols <- all_names(expr)
  symbol_list <- setNames(as.list(symbols), symbols)
  symbol_env <- chr2env(symbol_list, parent = f_env)

  # Known symbols
  greek_env <- clone_env(greek_env, parent = symbol_env)
}

test_that("Greek environment works", {
  expect_equal(to_math(pi), "\\pi")
  expect_equal(to_math(beta), "\\beta")
})

test_that("all_names works", {
  expect_equal(all_names(quote(x + y + f(a, b, c, 10))),
               c("x", "y", "a", "b", "c"))
})

test_that("Arbitrary names works", {
  expect_equal(to_math(x), "x")
  expect_equal(to_math(longvariablename), "longvariablename")
  expect_equal(to_math(pi), "\\pi")
})

test_that("Operators work", {
  expect_equal(to_math(sin(x + pi)), "\\sin(x + \\pi)")
  expect_equal(to_math(log(x_i ^ 2)), "\\log(x_i^2)")
  expect_equal(to_math(sin(sin)), "\\sin(sin)")
})

test_that("all_calls() works", {
  expect_equal(all_calls(quote(f(g + b, c, d(a)))),
               c("f", "+", "d"))
})

test_that("arbitrary functions are recognized", {
  expect_equal(to_math(f(a * b)), "\\mathrm{f}(a * b)")
})

# symbols = list(
#   "20" = c(
#     "1" = "!",
#     "2" = "\\forall",
#     "3" = "\\#",
#     "4" = "\\foreach",
#     "5" = "\\%",
#     "6" = "\\&",
#     "7" = "\\in",
#     "8" = "(",
#     "9" = ")",
#     "A" = "*",
#     "B" = "+",
#     "C" = ",",
#     "D" = "-",
#     "E" = ".",
#     "F" = "/"
#   ),
#   "30" = c(
#     setNames(as.character(0:9),
#              as.character(0:9)),
#     ":", ";", "<", "=", ">", "?"
#   ),
#   "40" = c(
#     "\\cong", "A", "B", "\\Chi",
#     "\\Delta", "E", "\\Phi", "\\Gamma",
#     "H", "I", "\\Theta", "K", "\\Lambda",
#     "M", "N", "O"
#   ),
#   "50" = c(
#     "\\Pi", "\\Theta", "P", "\\Sigma",
#     "T", "Y", "\\Zeta", "\\Omega", "\\Xi",
#     "\\Psi", "Z", "\\[", "\\therefore",
#     "\\]", "\\perp", "\\_"
#   ),
#   "60" = c(
#     # 600 is a radical extender
#     NA_character_, "\\alpha", "\\beta", "\\chi",
#     "\\delta", "\\varepsilon", "\\phi", "\\upsilon",
#     "\\eta", "\\iota", "\\phi", "\\kappa", "\\lambda",
#     "\\mu", "\\nu", "\\omicron"
#   ),
#   "70" = c(
#     "\\rho", "\\theta", "\\rho", "\\sigma", "\\tau",
#     "\\upsilon", #
#     "\\{", "|", "\\}", "\\~"
#   ),
#   "A0" = {
#   }
# )
