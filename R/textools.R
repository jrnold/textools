library(stringr)
library(purrr)


parens <- function(x) str_c("(", x, ")")
braces <- function(x) str_c("{", x, "}")
brackets <- function(x) str_c("[", x, "]")
math <- function(x) str_c("$", x, "$")


latex <- function(cmd, ..., brackets=NULL, trailing=TRUE) {
  args <- as.character(list(...))
  if (!is.null(brackets)) {
    brackets = as.character(brackets)
  }
  x <- list(cmd = as.character(cmd), args = args, brackets = brackets)
  class(x) <- "latex"
  x
}

as.character.latex <- function(x, trailing=TRUE) {
  brackets_str <- if (!is.null(x[["brackets"]]) &&
                             length(x[["brackets"]]) > 0) {
    str_c(map_chr(x[["brackets"]], brackets), collapse = "")
  } else {
    ""
  }
  braces_str <- if (!is.null(x[["args"]]) && length(x[["args"]]) > 0) {
    str_c(map_chr(x[["braces"]], braces), collapse = "")
  } else {
    if (trailing) {
      "{}"
    } else {
      ""
    }
  }
  str_c("\\", x[["cmd"]], brackets_str, braces_str)
}

print.latex <- function(x) {
  charx <- as.character(x)
  cat(charx)
  invisible(charx)
}

latex_command <- function(name) {
  function(args=NULL, optional=NULL) {
    if (is.null(optional)) {
      optional_str <- ""
    } else {
      optional_str <- brackets(as.character(optional))
    }
    if (is.null(args) || length(args) == 0) {
      args_str <- ""
    } else {
      args_str <- map_chr(as.character(args), brackets)
    }
    str_c("\\", name, optional_str, args_str)
  }
}

latex_environment <- function(name) {
  name <- as.character(name)[1]
  function(string, args=character(), optional=NULL) {
    str_c(latex_command("begin")(args = c(name, args), optional = optional),
          str_c(as.character(string), sep = "", collapse = "\n"),
          latex_command("end")(args = name))
  }
}



## Latex Command

latex_command <- function(name, definition, num=0, default=NULL,
                          cmd="providecommand") {
  if (!is.null(default)) {
    default <- as.character(default)
  }
  x <- list(name = as.character(name),
            definition = as.character(definition),
            num = as.integer(num),
            default = default,
            cmd = as.character(cmd))
  class(x) <- "latex_command"
  x
}

as.character.latex_command <- function(x, ...) {
  cmd <- "providecommand"
  num_str <- if (x[["num"]] > 0) {
    num_str <- brackets(x[["num"]])
  } else {""}
  opt_str <- if (!is.null(x[["default"]])) {
    opt_str <- brackets(x[["default"]])
  } else {""}
  str_c("\\", cmd,
        braces(str_c("\\", x[["name"]])),
        num_str,
        opt_str,
        braces(x[["definition"]]))
}

print.latex_command <- function(x) {
  xchar <- as.character(x)
  cat(xchar)
  invisible(xchar)
}

.latex_replace_args <- function(string, args) {
  for (i in seq_along(args)) {
    # latex args max out at 9, so don't worry about it
    string <- str_replace(string, str_c("#", i), args[i])
  }
  string
}

.latex_optional_arg <- function(args, optional = NULL, default=NULL) {
  if (!is.null(default)) {
    if (!is.null(optional)) {
      args <- c(as.character(optional), args)
    } else {
      args <- c(default, args)
    }
  }
  args
}


as.function.latex_command <- function(x) {
  function(..., optional=NULL) {
    args <- .latex_optional_arg(as.character(list(...)),
                                optional = optional,
                                default = x[["default"]])
    .latex_replace_args(x[["definition"]], args)
  }
}

## LaTeX Environment

latex_environment <- function(begin_def, end_def, num=0, default=NULL) {
  list(begin_def = as.character(begin_def),
       end_def = as.character(end_def),
       num = as.integer(num),
       default = as.character(default))
}

as.character.latex_environment <- function(x) {
  cmd <- "newenvironment"
  as.character(latex(name, begin_def, end_def, num = num, default = default))
}

as.function.latex_environment <- function(x) {
  function(string, ..., optional = NULL) {
    args <- .latex_optional_arg(as.character(list(...)),
                                optional = optional,
                                default = x[["default"]])
    str_c(.latex_replace_args(begin_def, args),
          as.character(string),
          .latex_replace_args(end_def, args))
  }
}

comment <- function(x, newline=TRUE) {
  nl <- if (newline) "\n" else ""
  str_c("% ", x, nl)
}

latex_macros <- function(name, value, sep="\n") {
  str_c("\\newcommand", brackets(name), brackets(value))
}

list_to_latex <- function(x, ...) {
  # LaTeX -> nothing
  # character -> escaped LaTeX
  # expression -> verbatim
  # integer, number -> math
  # markdown -> rendered to LaTeX
}

escape_latex <- function(x) {
  special_char <- c("{", "}", "#", "$", "&", "_", "%")
  special_char_pattern <-  str_c("[", str_c(special_char, collapse = ""), "]")
  print(special_char_pattern)
  x <- str_replace_all(x, str_c("(", special_char_pattern, ")"), "\\\\\\1")
  # backslashes that are not escaping special characters
  x <- str_replace_all(x, str_c('\\\\', "(?!", special_char_pattern, ")"),
                       '\\\\textbackslash{\\}')
  x <- str_replace_all(x, fixed("~"), "\\textasciitilde{}")
  x <- str_replace_all(x, fixed("^"), "\\textasciicircum{}")
  x
}

# Macros can only include letters
.valid_macro <- function(x) {
  str_detect(x, "^[A-Za-z]$")
}
