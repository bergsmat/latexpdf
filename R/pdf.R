#' Coerce to PDF
#'
#' Coerces to PDF. Generic, with methods for character, document, and data.frame.
#' @seealso as.pdf.character
#' @seealso as.pdf.document
#' @seealso as.pdf.data.frame
#' @param x object
#' @param ... passed arguments
#' @export
as.pdf <- function(x,...)UseMethod('as.pdf')

#' Coerce to PDF from Document
#'
#' Coerces to PDF from document. Makes a system call to 'pdflatex'. Extra arguments ignored.
#' @export
#' @describeIn as.pdf document method
#' @param stem the stem of a file name (no extension)
#' @param dir output directory
#' @param clean whether to delete system files after pdf creation
#' @seealso as.pdf.data.frame

as.pdf.document <- function(
  x,
  stem,
  dir='.',
  clean=TRUE,
  ...
){
  if(missing(stem))stop('a file stem (no extension) must be provided')
  if (contains('\\.pdf$',stem,ignore.case=TRUE)){
    warning('stripping .pdf from file stem ...')
    stem <- sub('\\.pdf$','',stem,ignore.case=TRUE)
  }
  outfile <- paste0(stem,'.tex')
  outpath <- file.path(dir,outfile)
  writeLines(x,outpath)
  cmd <- paste0('pdflatex -output-directory=',dir,' ',outpath)
  result <- system(cmd)
  variants <- paste0(stem,c('.tex','.log','.aux','.out'))
  possibles <- file.path(dir,variants)
  actuals <- possibles[file.exists(possibles)]
  if(clean)file.remove(actuals)
  invisible(result)
}

#' Coerce to PDF from Character
#'
#' Coerces character to document and then to pdf. Extra arguments passed to \code{\link{as.document.character}} and \code{\link{as.pdf.document}}.
#' @export
#' @describeIn as.pdf character method
#' @seealso tex2pdf
#' @seealso viewtex
as.pdf.character <- function(x,stem,...)as.pdf(as.document(x,...),stem=stem,...)


#' Coerce to PDF from Data Frame
#'
#' Coerces data.frame to document and then to pdf.
#' @export
#' @seealso as.pdf.character
#' @seealso as.pdf.document
#' @seealso as.document.data.frame
#' @seealso as.tabular.data.frame
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
#' @param rowcolors character vector of color names, recycled as necessary to color all rows (NULL: no color)
#' @param rowgrouplabel character string (at least one character) to label rowgroup column
#' @param charjust default justification for character columns
#' @param numjust default justification for numeric columns
#' @param justify manual specification of column justifications: left, right, center, or decimal (vector as long as ncol(x))
#' @param colwidth manual specification of column width. (vector of length ncol(x).) Overrides \code{justify where not NA.}
#' @param paralign used with colwidth to align paragraphs: top, middle, or bottom.
#' @param na string to replace NA elements
#' @param verbatim whether to use verbatim environment for numeric fields.  Makes sense for decimal justification; interacts with \code{trim} and \code{justify}.
#' @param escape symbol used by `verb' command as delimiter.  A warning is issued if it is found in non-NA text.
#' @param trim passed to the format command: true by default, so that alignment is the responsibility of just the tabular environment arguments
#' @param source optional source attribution
#' @param file optional file name
#' @param source.label optional text to preceed source if specified
#' @param file.label optional text to preceed file if specified
#' @param basefile if TRUE, strip path from file for display purposes
#' @param tabularEnvironment default \code{tabular}; consider also \code{longtable}
#' @param wider additional document width in mm
#' @param longer additional document lenth in mm
#' @param preamble latex markup to include before beginning the document
#' @param landscape if TRUE, default orientation is `landscape' not `portrait'
#' @param geoLeft geometry package: left margin
#' @param geoRight geometry package: right margin
#' @param geoTop geometry package: top margin
#' @param geoBottom geometry package: bottom margin
#' @param thispagestyle thispagestyle command
#' @param pagestyle pagestyle command
#' @param prolog latex markup to include before x
#' @param epilog latex markup to include after x
#' @param stem the stem of a file name (no extension)
#' @param dir output directory
#' @param clean whether to delete system files after pdf creation
#' @param footnote.size font size for source and file attributions
#' @param ... passed eventually to \code{\link{makePreamble}}

as.pdf.data.frame <- function(
  x, # tabular.data.frame
  rules=c(2,1,1), # tabular data.frame
  walls=0, # tabular.data.frame
  grid=FALSE, # tabular.data.frame
  rowgroups=factor(rownames(x)), # tabular.data.frame
  colgroups=factor(names(x)), # tabular.data.frame
  rowbreaks=if(grid)breaks(rowgroups,...)else 0, # tabular.data.frame
  colbreaks=if(grid)breaks(colgroups,...)else 0, # tabular.data.frame
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
  trim=TRUE,  # tabular.data.frame
  source=NULL,  # tabular.data.frame
  file=NULL, # tabular.data.frame
  source.label='source: ', # tabular.data.frame
  file.label='file: ', # tabular.data.frame
  basefile=FALSE, # tabular.data.frame
  tabularEnvironment='tabular', # tabular.data.frame
  footnote.size = 'tiny', # tabular.data.frame
  landscape=FALSE, # makePreamble
  geoLeft = '1mm', # makePreamble
  geoRight = '1mm', # makePreamble
  geoTop = '1mm', # makePreamble
  geoBottom = '1mm', # makePreamble
  wider=0, # as.document.data.frame
  longer=0, # as.document.data.frame
  preamble = makePreamble( # as.document.character
    landscape = landscape,
    geoLeft = geoLeft,
    geoRight = geoRight,
    geoTop = geoTop,
    geoBottom = geoBottom,
    ...
  ),
  thispagestyle=command('thispagestyle',args='empty'), # as.document.character
  pagestyle=command('pagestyle',args='empty'), # as.document.character
  prolog=NULL, # as.document.character
  epilog=NULL, # as.document.character
  stem, # as.pdf.document
  dir,# as.pdf.document
  clean,# as.pdf.document
  ... # passed to makePreamble
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
    preamble = preamble,
    wider = wider,
    longer = longer,
    thispagestyle = thispagestyle,
    pagestyle = pagestyle,
    prolog = prolog,
    epilog = epilog
  )
  as.pdf(doc, stem = stem, dir = dir, clean = clean)
}

#' Convert TEX to PDF
#'
#' Converts TEX to PDF.  \code{tex2pdf} accepts the file names of tex fragments. It reads those fragments, wraps them like documents and makes pdf files.
#' @export
#' @param x vector of file names
#' @param stem the stem of a file name (no extension)
#' @param dir output directory
#' @param clean whether to delete system files after pdf creation
#' @param onefile whether to combine tex snippets into a single file
#' @param ... passed to \code{\link{as.pdf.character}}
#' @seealso as.pdf.character
#' @seealso viewtex
tex2pdf <- function(
  x,
  stem=NULL,
  dir=NULL,
  clean=TRUE,
  onefile=FALSE,
  ...
){
  stopifnot(
    length(x)>0,
    all(file.exists(x)),
    length(stem)==1 | length(stem)==length(x) | stem==NULL,
    length(stem)==1 | onefile==FALSE | stem==NULL,
    length(dir)==1 | length(dir)==length(x) | dir==NULL
  )
  is.tex <- sapply(x,function(nm)contains('\\.tex',nm,ignore.case=TRUE))
  if(any(!is.tex))warning('x is expected to be a vector of tex file names')
  dat <- lapply(x,readLines)
  if(is.null(stem))stem <- sub('\\.[^.]+$','',basename(x),ignore.case=TRUE)
  if(is.null(dir))dir <- dirname(x)
  dir <- rep(dir,length.out=length(stem))
  if(onefile)stem <- stem[[1]]
  if(onefile)dir <- dir[[1]]
  if(onefile)dat <- list(unlist(dat))
  stopifnot(length(dat)==length(stem),length(dat)==length(dir))
  target <- paste0(stem,'_doc')
  for(index in seq_along(dat)){
    as.pdf.character(
      dat[[index]],
      stem=target[[index]],
      dir=dir[[index]],
      clean=clean,
      ...
    )
  }
  invisible(file.path(dir,paste0(target,'.pdf')))
}

#' Render and View TEX Files
#'
#' Renders and TEX files as PDF and opens them for viewing.
#' @export
#' @param x vector of file names
#' @param delete whether temporary pdf (_doc.pdf) should persist
#' @param latency how many seconds to wait before deleting temporary pdf
#' @param ... passed to \code{\link{tex2pdf}}
#' @seealso tex2pdf
#' @seealso as.pdf.character
#' @importFrom utils browseURL

viewtex <- function(x,delete=TRUE,latency=1,...){
  newfiles <- tex2pdf(x,...)
  sapply(newfiles,browseURL)
  if(delete)Sys.sleep(latency)
  if(delete)sapply(newfiles,unlink)
  invisible(newfiles)
}
