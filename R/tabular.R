#' Coerce to tabular
#'
#' Coerces to tabular.  Generic, with methods for data.frame, table, and matrix.
#' @param x object
#' @param ... passed arguments
#' @export
#' @seealso \code{\link{as.ltable}}
as.tabular <- function(x,...)UseMethod('as.tabular')

#' Coerce to tabular from data.frame
#'
#' Coerces to tabular from data.frame. Extra arguments passed to \code{\link{format.data.frame}}.
#' @export
#' @describeIn as.tabular data.frame method
#' @return tabular
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
#' @param decimal.mark passed to \code{\link{format.data.frame}}
#' @param colwidth manual specification of column width. (vector of length ncol(x).) Overrides \code{justify} where not NA.
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
#' @param footnote.size font size for source and file attributions
#' @examples
#' as.tabular(head(Theoph))
#' as.tabular(table(1:3,4:6))
#' as.tabular(head(Theoph,source='foo/bar',footnote.size='huge'))
#' \dontrun{as.pdf(head(Theoph))}

#'
as.tabular.data.frame <- function(
  x,
  rules=c(2,1,1),
  walls=0,
  grid=FALSE,
  rowgroups=factor(rownames(x)),
  colgroups=factor(names(x)),
  rowbreaks=if(grid)breaks(rowgroups)else 0,
  colbreaks=if(grid)breaks(colgroups)else 0,
  rowgrouprule = 0,
  colgrouprule = 0,
  rowcolors=NULL,
  rowgrouplabel=' ',
  charjust='left',
  numjust='right',
  justify=ifelse(sapply(x,is.numeric),numjust,charjust),
  decimal.mark = getOption('OutDec'),
  colwidth=NA,
  paralign='top',
  na='',
  verbatim=ifelse(sapply(x,is.numeric),TRUE,FALSE),
  escape='#',
  trim=TRUE,
  source=NULL,
  file=NULL,
  source.label='source: ',
  file.label='file: ',
  basefile=FALSE,
  tabularEnvironment='tabular',
  footnote.size = 'tiny',
  ...
){
  #groom arguments
  # shall there be row group labels and column group labels?
  groupcols <- inherits(colgroups, 'character')
  grouprows <- inherits(rowgroups, 'character')
  stopifnot(length(rowgrouprule) == 1, length(colgrouprule) == 1)
  x <- as.data.frame(x)
  rules <- rep(rules, length.out = 3)
  walls <- rep(walls, length.out = 2)
  rowgroups <- rep(rowgroups, length.out=nrow(x))
  colgroups <- rep(colgroups, length.out=ncol(x))
  rowbreaks <- rep(rowbreaks, length.out=nrow(x)-1)
  colbreaks <- rep(colbreaks, length.out=ncol(x)-1)
  if(!is.null(rowcolors))rowcolors <- rep(rowcolors, length.out=nrow(x))
  stopifnot(length(charjust)==1)
  stopifnot(length(numjust)==1)
  stopifnot(length(escape)==1)
  stopifnot(charjust %in% c('left','right','center'))
  stopifnot(numjust %in% c('left','right','center'))

  # if grouprows, rowgroups becomes a column, affecting following ncol
  if(grouprows){
   x[rowgrouplabel] <- rowgroups
   x <- shuffle(x,rowgrouplabel) # place first
  }
  partial <- paste0('\\cline{',2,'-',ncol(x),'}')
  multirow <- function(x){
    node <- runhead(x)
    blocks <- cumsum(node)
    extent <- reapply(blocks,blocks, length)
    y <- paste0('\\multirow{',extent,'}{*}{',x,'}')
    y[!node] <- ''
    y
  }
  if(grouprows) x[,1] <- multirow(x[,1])
  mitigate <- function(arg,cols,rowgroupval){
    if(length(arg) == 1) arg <- rep(arg, length.out=cols)
    if(length(arg) == cols - 1) arg <- append(arg,rowgroupval,0)
    stopifnot(length(arg) == cols)
    arg
  }
  colbreaks <- mitigate(colbreaks, ncol(x) - 1, rowgrouprule)
  na <- mitigate(na, ncol(x), '')
  verbatim <- as.logical(mitigate(verbatim,ncol(x),FALSE))
  paralign <- map(paralign,from=c('top','middle','bottom'),to=c('p','m','b'))[[1]]
  colwidth <- mitigate(colwidth,ncol(x), NA)
  colwidth <- sub('^',paste0(paralign,'{'),colwidth)
  colwidth <- sub('$','}',colwidth)
  justify <- mitigate(justify, ncol(x), 'left')
  decimal <- justify=='decimal'
  justify <- map(justify, from=c('left','right','center','decimal'),to=c('l','r','c','r'))
  justify[!is.na(colwidth)] <- colwidth[!is.na(colwidth)]
  format <- tabularformat(justify=justify, breaks=colbreaks, walls=walls) #ready
  header <- row2tabular(names(x)) #ready
  if(grouprows & groupcols) colgroups <- c('',colgroups)
  multicol <- function(x,colbreaks,justify,walls){
    node <- runhead(x)
    blocks <- cumsum(node)
    extent <- reapply(blocks,blocks, length)
    rightbreaks <- c(colbreaks,walls[[2]])
    leftbreaks <- c(walls[[1]],rep('0',length(justify) - 1))
    rightbreaks <- sapply(rightbreaks,function(b)paste(collapse='',rep('|',as.numeric(b))))
    leftbreaks  <- sapply(leftbreaks,function(b)paste(collapse='',rep('|',as.numeric(b))))
    battery <- data.frame(
      stringsAsFactors=FALSE,
      blocks=blocks,
      extent=extent,
      leftbreaks=leftbreaks,
      justify=justify,
      rightbreaks=rightbreaks,
      x=x
    )
    # the relevant break on any multicol is that associated with the last member of the block
    battery$justify <- 'c'
    battery$rightbreaks <- last(battery$rightbreaks,within=battery$blocks)
    y <- with(battery, paste0('\\multicolumn{',extent,'}{',leftbreaks,justify,rightbreaks,'}{',x,'}'))
    y[!node] <- ''
    y
  }
  if(groupcols) colgroups <- multicol(colgroups,colbreaks,justify,walls)
  header2 <- row2tabular(colgroups[colgroups!=''])

  sapply(names(x)[verbatim],function(nm)if(any(!is.na(x[[nm]]) & contains(escape,x[[nm]],fixed=TRUE)))warning(nm,'contains', escape))
  x[] <- lapply(seq_along(x),function(col)if(decimal[[col]])align.decimal(x[[col]],decimal.mark = decimal.mark)else format(x[[col]],trim=trim,decimal.mark = decimal.mark,...))
  x[] <- lapply(seq_along(x),function(col)sub('^ *NA *$',na[[col]],x[[col]]))
  x[] <- lapply(seq_along(x),function(col)if(verbatim[[col]])paste0('\\verb',escape,x[[col]],escape)else x[[col]])
  x <- as.matrix(x)
  x <- apply(x,1,row2tabular) #ready
  # we now have a format string, a header, and all rows as character vector
  # splice in the horizontal rules
  # we treat header as a row, and treat rules[2:3] as pertaining to first and last row.
  # we create an empty row to represent pre-header
  if(!is.null(rowcolors))x <- paste0('\\rowcolor{',rowcolors,'} ',x)
  x <- c('',if(groupcols) header2, header,x)
  oldbreaks <- rowbreaks
  rowbreaks <- c(rules[[1]],if(groupcols) colgrouprule,rules[[2]], rowbreaks,rules[[3]])
  stopifnot(length(rowbreaks)==length(x))
  # the line end style depends on position in a rowgroup block.  Only end-of-block
  # may have a full line.
  full <- '\\hline'
  rowgroupstyle <- rev(runhead(rev(rowgroups)))
  # last of these is irrelevant, as 'rules' controls final aesthetic
  rowgroupstyle <- rowgroupstyle[-length(rowgroupstyle)]
  rowgroupstyle <- ifelse(rowgroupstyle,full,partial)

  style <- c(
    full, # top
    if(groupcols) full, # below grouping labels
    full, # below regular labels
    rowgroupstyle, # regular data
    full # bottom
  )
  while(any(rowbreaks > 0)){
    x[rowbreaks > 0] <- paste(x[rowbreaks > 0],style[rowbreaks > 0])
    rowbreaks <- rowbreaks - 1
  }
  x <- wrap(x,tabularEnvironment,args=format)
  class(x) <- c('tabular',class(x))
    if(!is.null(source))if(!is.null(source.label)) x <- c(x,paste0('\\\\{\\',footnote.size,' ',source.label,source,'}'))
  if(!is.null(file))if(!is.null(file.label)) x <- c(
    x,
    paste0(
      '\\\\{\\',footnote.size,' ',
      file.label,
      if(basefile)basename(file) else file,
      '}'
    )
  )
  if(is.null(file))return(x)
  else{
      writeLines(x,file)
      invisible(x)
  }
}

#' Coerce to tabular from table
#'
#' Coerces to tabular from table.
#' @export
#' @describeIn as.tabular table method
as.tabular.table <- function(x, ...){
  if(length(dim(x)) != 2) stop('tabular.table only implemented for 2-dimensional tables')
  class(x) <- 'matrix'
  as.tabular(x, ...)
}

#' Coerce to tabular from matrix
#'
#' Coerces to tabular from matrix.
#' @export
#' @describeIn as.tabular matrix method

as.tabular.matrix <- function(x,...){
  y <- as.data.frame(x)
  dimnames <- dimnames(x)
  if(!is.null(dimnames)){
    nms <- names(dimnames)
    rows <- nms[[1]]
    if(!is.null(rows))
    if(!is.na(rows))
    if(rows != '')
    if(!rows %in% names(y)){
      y[,rows] <- rownames(x)
      y <- shuffle(y, rows) # move to front
    }
  }
  as.tabular(y,...)
}

