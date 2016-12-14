#' Gunter Milde [unimathsymbols.txt](http://milde.users.sourceforge.net/LUCR/Math/) is
#'   one of the most complete and has both latex -> unicode, and unicode -> latex.
#' - http://www.johndcook.com/blog/2013/02/18/unicode-to-latex/. The conversion is
#'    in http://www.johndcook.com/data.js.
#'    The Python package [unicode_tex](https://pypi.python.org/pypi/unicode_tex/0.1.1)
#'    applies something similar.
#' - Julia [latex_symbols.jl](https://github.com/JuliaLang/julia/blob/2751493abfae6ffe6b833b06c7950887a9379fbc/base/latex_symbols.jl) uses
#'    both the Unicode XML and unicode-math data with some manual additions.
#'    For unicode to latex see [unicode input](https://github.com/JuliaLang/julia/blob/a304c557928d9eaa0a28af5fe96327d5da70d14b/doc/src/manual/unicode-input.md).
#'    Also see the [vim mode](https://github.com/JuliaEditorSupport/julia-vim/blob/077f2369934748d7f3751061c02fb9496e0ac58a/doc/julia-vim-L2U-table.txt) or
#'    [emacs mode](https://github.com/JuliaEditorSupport/julia-emacs) for a latex-unicode generation script.
#' - [Better bibtex](https://github.com/retorquere/zotero-better-bibtex/blob/a3db6df76ff5ec5f9915f00d191784deefcfeb12/lib/unicode_table.rb).
#'     And the SQLite database is qunicode.mapping.
#'     It is translation from the Milde.
#' - [pylatexenc](https://pypi.python.org/pypi/pylatexenc/0.9) uses a small custom set of mappings latex -> unicode, 
#'     and unicode -> latex. Updated 2015. 1 star.
#' - https://pypi.python.org/pypi/latex/0.6.4
#' - https://github.com/inukshuk/latex-decode
#' - https://github.com/jmeas/latex-to-unicode.js
#' - https://github.com/inukshuk/latex-decode
#'
#'
library("readr")
STIX <- "http://www.ams.org/STIX/bnb/stix-tbl.ascii-2006-10-20"
#' Layout http://www.ams.org/STIX/bnb/stix-tbl.layout-2004-06-18



UNICODE <- "https://www.w3.org/Math/characters/unicode.xml"

library("xml2")
