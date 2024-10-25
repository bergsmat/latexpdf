#' Make a Preamble for a LaTeX Document
#'
#' Makes a preamble for a LaTeX Document.
#' @export
#' @return character
#' @param landscape if TRUE, default orientation is `landscape' not `portrait'
#' @param wide page width in mm
#' @param long page lenth in mm
#' @param geoLeft geometry package: left margin
#' @param geoRight geometry package: right margin
#' @param geoTop geometry package: top margin
#' @param geoBottom geometry package: bottom margin
#' @param documentclass document class command
#' @param xcolorPackage xcolor package command
#' @param geometryPackage geometry package command
#' @param geometry geometry specification
#' @param multirow multirow specification
#' @param float float specification
#' @param longtable longtable specification
#' @param inputenc input encoding
#' @param fontenc output encoding
#' @param morePreamble additional preamble before beginning the document
#' @param ... ignored
#' @examples
#' makePreamble()

makePreamble <- function(
  landscape=FALSE,
  wide=if(landscape) 279.4 else 215.9,
  long=if(landscape) 215.9 else 279.4,
  geoLeft = '1mm',
  geoRight = '1mm',
  geoTop = '1mm',
  geoBottom = '1mm',
  documentclass = command('documentclass',args='article'),
  xcolorPackage = command('usepackage',options=list(
   # 'usenames', 2023-12-18 usenames obsolete
    'dvipsnames', 
    'svgnames','table'),args='xcolor'),
  geometryPackage = command('usepackage',options=list(left=geoLeft,top=geoTop,bottom=geoBottom,right=geoRight),args='geometry'),
  geometry = command('geometry',args=list(paste0('papersize=',paste0('{',wide,'mm',',',long,'mm}')))),
  multirow = command('usepackage',args='multirow'),
  float = command('usepackage',args='float'),
  longtable = command('usepackage',args='longtable'),
  inputenc = command("usepackage", options="utf8", args="inputenc"),
  fontenc = command("usepackage", options="T1", args="fontenc"),
  morePreamble = NULL,
  ...
)c(
  documentclass,
  xcolorPackage,
  geometryPackage,
  geometry,
  multirow,
  float,
  longtable,
  inputenc,
  fontenc,
  morePreamble
)
#' Coerce to LaTeX Document
#'
#' Coerces to LaTeX document.  Generic, with methods for character and data.frame.
#' @export
#' @return character
as.document <- function(x,...)UseMethod('as.document')

#' Coerce to LaTeX Document from Character
#'
#' Coerces to LaTex document from character.
#' @export
#' @return character
#' @param preamble latex markup to include before beginning the document
#' @param thispagestyle thispagestyle command
#' @param pagestyle pagestyle command
#' @param prolog latex markup to include before x
#' @param epilog latex markup to include after x
#' @describeIn as.document character method
as.document.character <- function(
	x,
	preamble=makePreamble(...),
	thispagestyle=command('thispagestyle',args='empty'),
	pagestyle=command('pagestyle',args='empty'),
	prolog=NULL,
	epilog=NULL,
	...
){
	content <- c(
	    thispagestyle,
	    pagestyle,
	    prolog,
	    x,
	    epilog
       )
       body <- wrap(environment='document', content)
       doc <- c(preamble, body)
       class(doc) <- c('document',class(doc))
       doc
}

#' Coerce to LaTeX Document from Data Frame
#'
#' Coerces to LaTeX document from data.frame.
#' @export
#' @return character
#' @describeIn as.document data.frame method
#' @param x object to be converted, typically data.frame (paths of tex files for \code{tex2pdf} and \code{viewtex})
#' @param rules numeric; will be recycled to length 3.  indicates number of horizontal lines above and below the header, and below the last row.
#' @param walls numeric, recycled to length 2.  Number of vertical lines on left and right of table.
#' @param grid logical, whether to have lines between rows and columns
#' @param rowgroups a vector as long as nrow(x), non-repeats trigger horizontal lines
#' @param colgroups a vector as long as names(x), non-repeats trigger vertical lines
#' @param rowbreaks numeric: a manual way to specify numbers of lines between rows (ignores grid and rowgroups)
#' @param colbreaks numeric: a manual way to specify numbers of lines between columns (ignores grid and colgroups)
# @param rowgrouprule number of lines to set off row group column, if rowgroups supplied as character
# @param colgrouprule number of lines to set off col group header, if colgroups supplied as character
#' @param rowcolors character vector of color names, recycled as necessary to color all rows (NULL: no color)
# @param rowgrouplabel character string (at least one character) to label rowgroup column
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
#' @param wide nominal page width in mm
#' @param long nominal page length in mm
#' @param wider additional page width in mm
#' @param longer additional page lenth in mm
#' @param ... passed to \code{\link{as.tabular.data.frame}} and \code{\link{as.document.character}}
#' @seealso \code{\link{as.tabular.data.frame}}
#' @seealso \code{\link{as.document.character}}
#' @seealso \code{\link{as.pdf.data.frame}}
#' @examples
#' as.document(head(Theoph))

as.document.data.frame <- function(
	  x,
	  rules = c(2, 1, 1),
	  walls = 0,
	  grid = FALSE,
	  rowgroups = factor(rownames(x)),
	  colgroups = factor(names(x)),
	  rowbreaks = if (grid) breaks(rowgroups, ...) else 0,
	  colbreaks = if (grid) breaks(colgroups, ...) else 0,
	  rowcolors=NULL,
	  charjust = "left",
	  numjust = "right",
	  justify = ifelse(sapply(x, is.numeric), numjust, charjust),
	  colwidth = NA,
	  paralign = "top",
	  na = "",
	  verbatim = ifelse(sapply(x, is.numeric), TRUE, FALSE),
	  escape = "#",
	  reserve = TRUE,
	  trim = TRUE,
	  wide = NULL,
	  long = NULL,
	  wider = 0,
	  longer = 0,
	  ...
){
  if(ncol(x) == 0) stop('need at least one column')
  stopifnot(inherits(x,'data.frame'))
  rules <- rep(rules, length.out = 3)
  walls <- rep(walls, length.out = 2)
  if(nrow(x)) rowbreaks <- rep(rowbreaks, length.out = nrow(x) - 1)
  colbreaks <- rep(colbreaks, length.out = ncol(x) - 1)
  text <- maxChar(do.call(paste,fixedwidth.data.frame(x)))
  if(!nrow(x)) text <- sum(nchar(names(x))) + length(x) - 1
  bars <- c(walls,colbreaks)
  bars <- sum(bars) + sum(bars[bars>1]-1)*4
  #bars[bars>1] - 1 gives the number of inter-bar gaps, which are about 4 times as wide as a bar.
  #same logic applies to lines.

  if(is.null(wide))wide <- text * 2.36 + bars*0.14 + 5.9 + wider

  rows <- 1+nrow(x)
  lines <- c(rules,rowbreaks)
  lines <- sum(lines) + sum(lines[lines>1]-1)*4

  if(is.null(long))long <- rows*4.21 + lines*0.16 + 2 + longer

  tab <- as.tabular(
          x=x,
          rules=rules,
          walls=walls,
          grid=grid,
          rowgroups=rowgroups,
          colgroups=colgroups,
          rowbreaks=rowbreaks,
          colbreaks=colbreaks,
          rowcolors=rowcolors,
          charjust=charjust,
          numjust=numjust,
          justify=justify,
          colwidth=colwidth,
          paralign=paralign,
          na=na,
          verbatim=verbatim,
          escape=escape,
          reserve=reserve,
          trim=trim,
          ...
  )
  doc <-  as.document(tab,wide=wide,long=long,...)
  doc
}
