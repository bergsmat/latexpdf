#' Coerce to PDF
#'
#' Coerces to PDF. Generic, with methods for character, document, and data.frame.
#' @seealso \code{\link{as.pdf.character}}
#' @seealso \code{\link{as.pdf.document}}
#' @seealso \code{\link{as.pdf.data.frame}}
#' @seealso \code{\link{as.png}}
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
#' @return the output file path (invisible)

as.pdf.document <- function(
  x,
  stem = 'latexpdf-doc',
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
  hopeful <- paste0(stem,'.pdf')
  outpath <- file.path(dir,outfile)
  expects <- file.path(dir,hopeful)
  writeLines(x,outpath)
  cmd <- paste0('pdflatex -output-directory=',dir,' ',outpath)
  result <- tryCatch(error = function(e)e, system(cmd))
  variants <- paste0(stem,c('.tex','.log','.aux','.out'))
  possibles <- file.path(dir,variants)
  actuals <- possibles[file.exists(possibles)]
  if(clean)file.remove(actuals)
bad <- inherits(result,'try-error') || !file.exists(expects)
  if(bad) stop('could not make ', expects)
  invisible(expects)
}

#' Coerce to PDF from Character
#'
#' Coerces character to document and then to PDF. Extra arguments passed to \code{\link{as.document.character}} and \code{\link{as.pdf.document}}.
#' @export
#' @describeIn as.pdf character method
#' @seealso \code{\link{tex2pdf}}
#' @seealso \code{\link{viewtex}}
as.pdf.character <- function(x,stem,...)as.pdf(as.document(x,...),stem=stem,...)


#' Convert TEX to PDF
#'
#' Converts TEX to PDF.  \code{tex2pdf} accepts the file names of TEX fragments. It reads those fragments, wraps them like documents and makes PDF files.
#' @export
#' @param x vector of file names
#' @param stem the stem of a file name (no extension)
#' @param dir output directory
#' @param clean whether to delete system files after PDF creation
#' @param onefile whether to combine TEX snippets into a single file
#' @param ... passed to \code{\link{as.pdf.character}}
#' @seealso \code{\link{as.pdf.character}}
#' @seealso \code{\link{viewtex}}
#' @return invisible vector of paths to created files
#'
#' @examples
#' file <- file.path(tempdir(),'test.tex')
#' writeLines(as.ltable(head(Theoph)), file)
#' tex2pdf(file)
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
#' @param latency how many seconds to wait before deleting temporary PDF,
#' @param png view as png instead of pdf
#' @param ... passed to \code{\link{tex2pdf}}
#' @seealso \code{\link{tex2pdf}}
#' @seealso \code{\link{tex2png}}
#' @seealso \code{\link{as.pdf.character}}
#' @importFrom utils browseURL
#' @examples
#' file <- file.path(tempdir(),'test.tex')
#' writeLines(as.ltable(head(Theoph)), file)
#' \dontrun{
#' viewtex(file)
#' viewtex(file, png = TRUE, gs_cmd = 'mgs')
#' }

viewtex <- function(x,delete=TRUE,latency=1, png=FALSE,...){
  newfiles <- if(png)tex2png(x,...) else tex2pdf(x,...)
  sapply(newfiles,browseURL)
  if(delete)Sys.sleep(latency)
  if(delete)sapply(newfiles,unlink)
  invisible(newfiles)
}
