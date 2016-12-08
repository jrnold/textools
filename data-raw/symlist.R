#' Get the list of symbols from the [Comprehensive LaTeX Symbol List](https://www.ctan.org/tex-archive/info/symbols/comprehensive/?lang=en)
#'
#' Maintainer Scott Pakin
#' License is https://www.ctan.org/license/lppl1.3
#'
#'
#'
#' ALSO see https://www.ctan.org/tex-archive/info/symbols/math
#'
#' http://milde.users.sourceforge.net/LUCR/Math/
#'
#' mathabx, mathdesign,

URL = "http://ctan.math.utah.edu/ctan/tex-archive/info/symbols/comprehensive/SYMLIST"
latex_symlist <- readLines(url(URL))

