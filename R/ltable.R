#' Coerce to LaTeX Table Environment
#'
#' Coerces to LaTeX table environment.  Generic, with methods for data.frame, table, and matrix.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @seealso \code{\link{as.tabular}}
as.ltable <- function(x,...)UseMethod('as.ltable')

#' Coerce to LaTeX table from data.frame
#'
#' Coerces to LaTeX table from data.frame. See details.
#'
#' Converts data.frame to tabular, then wraps it in specified environments, then wraps result in a latex table environment.  Result is returned visibly, or if \code{file} is specified it is printed to file and returned invisibly.

#' If \code{source} and \code{source.label} are defined, they will be printed in a tiny font immediately under the table (bound to the tabular element).  If \code{file} and \code{file.label} are defined as well, they will be printed (tiny) under source.  Set \code{source.label} to NULL to suppress embedding of \code{source}; set to empty string to suppress source label. Set \code{file.label} to NULL to suppress embedding of \code{file}; set to empty string to suppress file label. Note that \code{file} controls file destination, whether or not represented in the result.
#'
#' Extra arguments(\dots) are passed to \code{\link{as.tabular}}.



#' @param caption full version of the caption
#' @param cap short version of the caption, for list of tables
#' @param cap.top Should caption be placed at the top, instead of bottom?
#' @param label optional label
#' @param options options for latex table environment, e.g. H or !htpb
#' @param environments extra environments to nest between \sQuote{table} and \sQuote{tabular}
#' @param source optional source attribution
#' @param file optional file name
#' @param source.label optional text to preceed source if specified
#' @param file.label optional text to preceed file if specified
#' @param basefile if TRUE, strip path from file for display purposes
#' @param footnote.size font size for source and file, etc.
#' @return character
#' @export
#' @seealso \code{\link{as.tabular}}
#' @describeIn as.ltable data.frame method
#' @examples
#' as.ltable(head(Theoph))
#' as.ltable(table(1:3,4:6))

as.ltable.data.frame <- function(
  x,
  caption=NULL,
  cap=caption,
  cap.top=TRUE,
  label=NULL,
  options='H',
  environments='center',
  source=NULL,
  file=NULL, ### file needs to support verbatim e.g. _
  source.label='source: ',
  file.label='file: ',
  basefile=FALSE,
  footnote.size='tiny',
  ...
){
  x <- as.tabular(x, ...)
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
  for (env in environments) x <- wrap(x,env)
  if (!is.null(label))label <- command('label',args=label)
  if(!is.null(caption)){
    caption <- command(
      'caption',
      options=cap,
      args=paste(caption,label)
    )
    x <- c(
      if(cap.top)caption else NULL,
      x,
      if(!cap.top)caption else NULL
    )
  }
  x <- wrap(
    x,
    'table',
    options=options
  )
  class(x) <- c('ltable',class(x))
  if(is.null(file))return(x)
  else {
    writeLines(x,file)
    invisible(x)
  }
}

#' Coerce to ltable from table
#'
#' Coerces to ltable from table, reclassifying its argument as matrix.
#' @describeIn as.ltable table method
#' @export
as.ltable.table <- function(x, ...){
  if(length(dim(x)) != 2) stop('as.ltable.table only implemented for 2-dimensional tables')
  class(x) <- 'matrix'
  as.ltable(x, ...)
}

#' Coerce to ltable from matrix
#'
#' Coerces to ltable from matrix; tires to capture the column names as a caption, and (like \code{as.tabular.matrix}) converts its argument to data.frame, capturing rownames as a column in the first position if rownames are suitably named.
#' @describeIn as.ltable matrix method
#' @export
as.ltable.matrix <- function(x, caption = names(dimnames(x))[[2]],...){
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
  as.ltable(y,caption=caption, ...)
}
