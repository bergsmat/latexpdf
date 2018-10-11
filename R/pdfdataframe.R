#' Coerce to PDF from Data Frame
#'
#' Coerces data.frame to document and then to PDF.
#' Extra arguments are passed to \code{\link{makePreamble}}.
#' \code{\link{as.document.data.frame}} will try to guess an
#' appropriate width and length( \code{wide}, \code{long})
#' for the page, but you may need something \code{wider} or
#' \code{longer}, especially if you adjust aesthetics.
#' Negative values for \code{wider} and \code{longer} are meaningful.
#' @export
#' @seealso \code{\link{as.pdf.character}}
#' @seealso \code{\link{as.pdf.document}}
#' @seealso \code{\link{as.document.data.frame}}
#' @seealso \code{\link{as.tabular.data.frame}}
#' @seealso \code{\link{as.png.data.frame}}
#' @param x data.frame
#' @param rules numeric; will be recycled to length 3.  indicates number of horizontal lines above and below the header, and below the last row.
#' @param walls numeric, recycled to length 2.  Number of vertical lines on left and right of table.
#' @param grid logical, whether to have lines between rows and columns
#' @param rowgroups a vector as long as nrow(x), non-repeats trigger horizontal lines
#' @param colgroups a vector as long as names(x), non-repeats trigger vertical lines
#' @param rowbreaks numeric: a manual way to specify numbers of lines between rows (ignores grid and rowgroups)
#' @param colbreaks numeric: a manual way to specify numbers of lines between columns (ignores grid and colgroups)
#' @param rowgrouprule number of lines to set off row group column, if rowgroups supplied as character
#' @param colgrouprule number of lines to set off col group header, if colgroups supplied as character
#' @param rowcolors character vector of color names, recycled as necessary to color all rows (NULL: no color); not compatible with rowgroups
#' @param rowgrouplabel character string (at least one character) to label rowgroup column
#' @param charjust default justification for character columns
#' @param numjust default justification for numeric columns
#' @param justify manual specification of column justifications: left, right, center, or decimal (vector as long as ncol(x))
#' @param colwidth manual specification of column width. (vector of length ncol(x).) Overrides \code{justify where not NA.}
#' @param paralign used with colwidth to align paragraphs: top, middle, or bottom.
#' @param na string to replace NA elements
#' @param verbatim whether to use verbatim environment for numeric fields.  Makes sense for decimal justification; interacts with \code{trim} and \code{justify}.
#' @param escape symbol used by `verb' command as delimiter.  A warning is issued if it is found in non-NA text.
#' @param reserve substitute escape sequences for LaTeX \href{https://en.wikibooks.org/wiki/LaTeX/Basics#Reserved_Characters}{reserved} characters
#' @param trim passed to the format command: true by default, so that alignment is the responsibility of just the tabular environment arguments
#' @param source optional source attribution
#' @param file optional file name
#' @param source.label optional text to preceed source if specified
#' @param file.label optional text to preceed file if specified
#' @param basefile if TRUE, strip path from file for display purposes
#' @param tabularEnvironment default \code{tabular}; consider also \code{longtable}
#' @param footnote.size font size for source and file attributions
#' @param geoLeft geometry package: left margin
#' @param geoRight geometry package: right margin
#' @param geoTop geometry package: top margin
#' @param geoBottom geometry package: bottom margin
#' @param wide nominal page width in mm
#' @param long nominal page length in mm
#' @param wider additional page width in mm
#' @param longer additional page lenth in mm
#' @param thispagestyle thispagestyle command
#' @param pagestyle pagestyle command
#' @param prolog latex markup to include before x
#' @param epilog latex markup to include after x
#' @param stem the stem of a file name (no extension)
#' @param dir output directory
#' @param clean whether to delete system files after pdf creation
#' @param ... passed eventually to \code{\link{makePreamble}} and \code{\link{reserve}}
#' @examples
#' \dontrun{as.pdf(head(Theoph))}

as.pdf.data.frame <- function(
  x, # tabular.data.frame
  rules=c(2,1,1), # tabular data.frame
  walls=0, # tabular.data.frame
  grid=FALSE, # tabular.data.frame
  rowgroups=factor(rownames(x)), # tabular.data.frame
  colgroups=factor(names(x)), # tabular.data.frame
  rowbreaks=if(grid)breaks(rowgroups)else 0, # tabular.data.frame
  colbreaks=if(grid)breaks(colgroups)else 0, # tabular.data.frame
  rowgrouprule = 0, # tabular.data.frame
  colgrouprule = 0, # tabular.data.frame
  rowcolors=NULL, # tabular.data.frame
  rowgrouplabel=' ', # tabular.data.frame
  charjust='left', # tabular.data.frame
  numjust='right', # tabular.data.frame
  justify=ifelse(sapply(x,is.numeric),numjust,charjust), # tabular.data.frame
  colwidth=NA, # tabular.data.frame
  paralign='top', # tabular.data.frame
  na='', # tabular.data.frame
  verbatim=ifelse(sapply(x,is.numeric),TRUE,FALSE), # tabular.data.frame
  escape='#', # tabular.data.frame
  reserve = TRUE,
  trim=TRUE,  # tabular.data.frame
  source=NULL,  # tabular.data.frame
  file=NULL, # tabular.data.frame
  source.label='source: ', # tabular.data.frame
  file.label='file: ', # tabular.data.frame
  basefile=FALSE, # tabular.data.frame
  tabularEnvironment='tabular', # tabular.data.frame
  footnote.size = 'tiny', # tabular.data.frame
  geoLeft = '1mm', # makePreamble
  geoRight = '1mm', # makePreamble
  geoTop = '1mm', # makePreamble
  geoBottom = '1mm', # makePreamble
  wide = NULL, # as.document.data.frame
  long = NULL, # as.document.data.frame
  wider=0, # as.document.data.frame
  longer=0, # as.document.data.frame
  thispagestyle=command('thispagestyle',args='empty'), # as.document.character
  pagestyle=command('pagestyle',args='empty'), # as.document.character
  prolog=NULL, # as.document.character
  epilog=NULL, # as.document.character
  stem = 'latexpdf-doc', # as.pdf.document
  dir = '.',# as.pdf.document
  clean = TRUE,# as.pdf.document
  ... # passed to makePreamble, etc
){
  doc <- as.document(
    x,
    rules = rules,
    walls = walls,
    grid = grid,
    rowgroups = rowgroups,
    colgroups = colgroups,
    rowgrouprule = rowgrouprule,
    colgrouprule = colgrouprule,
    rowcolors = rowcolors,
    rowgrouplabel = rowgrouplabel,
    charjust = charjust,
    numjust = numjust,
    justify = justify,
    colwidth = colwidth,
    paralign = paralign,
    na = na,
    verbatim = verbatim,
    escape = escape,
    trim = trim,
    source = source,
    file = file,
    source.label = source.label,
    file.label = file.label,
    basefile = basefile,
    tabularEnvironment = tabularEnvironment,
    footnote.size = footnote.size,
    geoLeft = geoLeft,
    geoRight = geoRight,
    geoTop = geoTop,
    geoBottom = geoBottom,
    wide = wide,
    long = long,
    wider = wider,
    longer = longer,
    thispagestyle = thispagestyle,
    pagestyle = pagestyle,
    prolog = prolog,
    epilog = epilog,
    ...
  )
  as.pdf(doc, stem = stem, dir = dir, clean = clean, ...)
}
