#' Create LaTeX Code and PDF Documents.
#'
#' \pkg{latexpdf} helps you create pdf documents in R using
#' LaTeX techniques; this is especially useful for making stand-alone
#' PDF images of data.frames.  For report-length PDF, some flavor
#' of markup or markdown (e.g. Sweave, Rmarkdown) is probably a more
#' attractive mechanism for the main document; still, \pkg{latexpdf}
#' can be used to embed tables with the same aesthetics as stand-alone
#' versions.
#'
#' "Anything in LaTeX can be expressed in terms of commands and environments",
#' (<http://en.wikibooks.org/wiki/LaTeX/Absolute_Beginners>). Accordingly,
#' \pkg{latexpdf} provides R functions to generate LaTeX commands (command()) and
#' environments (wrap()).  For commands, care is taken to support options
#' and arguments. These can be used to create character vectors containing
#' arbitrary LaTeX code.
#'
#' In fact, the package itself uses these functions to convert data frames
#' to LaTeX code, providing reasonable defaults and supporting many
#' aesthetic interventions.  See '?as.tabular' and '?as.ltable' (tabular and
#' table environments, respectively).  See also 'vignette('tabular')
#' for a demonstration of options.
#'
#' While \pkg{latexpdf} can be useful for low-level operations, creating PDF
#' documents directly is more powerful.
#' Pre-generated LaTeX code can be inserted into literate programming
#' documents (Sweave, Rmarkdown) or can be auto-converted to stand-alone
#' PDF documents (see especially '?as.pdf.data.frame'). Functions tex2pdf()
#' and viewtex() create and visualize arbitrary tex code by converting to
#' PDF documents. They rely on as.pdf.document(), which places a system call
#' to 'pdflatex'.
#'
#' @docType package
#' @name latexpdf-package
#' @author Tim Bergsma, \email{bergsmat@gmail.com}

NULL
