#' Coerce to PNG
#'
#' Coerces to PNG. Generic, with methods for character, document, and data.frame.
#' @seealso \code{\link{as.png.character}}
#' @seealso \code{\link{as.png.document}}
#' @seealso \code{\link{as.png.data.frame}}
#' @seealso \code{\link{as.pdf}}
#' @param x object
#' @param ... passed arguments
#' @export
as.png <- function(x,...)UseMethod('as.png')

#' Coerce to PNG from Document
#'
#' Coerces to PNG from document. Makes a system call to 'pdflatex', converts resulting file to PNG. Extra arguments passed to \code{\link{as.pdf.document}} and \code{\link{ghostconvert}}.
#' @export
#' @describeIn as.png document method
#' @param stem the stem of a file name (no extension)
#' @param dir output directory
#' @param clean whether to delete system files after PNG creation
#' @param replace whether to delete the pdf file
#' @param multipage whether to convert multiple pages of the PDF
#' @seealso as.pdf.document
#' @seealso ghostconvert

as.png.document <- function(
  x,
  stem = 'latexpdf-doc',
  dir='.',
  clean=TRUE,
  replace = TRUE,
  multipage = TRUE,
  ...
){
  result <- as.pdf(x, stem = stem, dir = dir, clean = clean, ...)
  ghostconvert(result, replace = replace, multipage = multipage, ...)
}

#' Coerce to PNG from Character
#'
#' Coerces character to document, PDF, and then PNG. Extra arguments passed to \code{\link{as.document.character}} and \code{\link{as.png.document}}.
#' @export
#' @describeIn as.png character method
#' @seealso \code{\link{tex2png}}
#' @seealso \code{\link{viewtex}}
#' @seealso \code{\link{ghostconvert}}
as.png.character <- function(x,stem,...)as.png(as.document(x,...),stem=stem,...)


#' Convert TEX to PNG
#'
#' Converts TEX to PNG.  \code{tex2png} accepts the file names of TEX fragments. It reads those fragments, wraps them like documents and makes PNG files (converted from PDF files).
#' @export
#' @param x vector of file names
#' @param stem the stem of a file name (no extension)
#' @param dir output directory
#' @param clean whether to delete system files after PNG creation
#' @param onefile whether to combine tex snippets into a single file
#' @param replace whether to delete the intermediate PDF files
#' @param ... passed to \code{\link{tex2pdf}} and \code{\link{ghostconvert}}
#' @seealso \code{\link{as.png.character}}
#' @seealso \code{\link{viewtex}}
#' @examples
#' \dontrun{
#' file <- file.path(tempdir(),'test.tex')
#' writeLines(as.ltable(head(Theoph)), file)
#' tex2png(file, gs_cmd = 'mgs')
#' }
tex2png <- function(
  x,
  stem=NULL,
  dir=NULL,
  clean=TRUE,
  onefile=FALSE,
  replace=TRUE,
  ...
){
  result <- tex2pdf(x, stem = stem, dir = dir, clean = clean, onefile = onefile, ...)
  result <- sapply(result, ghostconvert, replace = replace, ...)
  invisible(result)
}

#' Call Ghostscript.
#'
#' Call Ghostscript, converting by default from PDF to PNG.
#' @export
#' @importFrom tools find_gs_cmd
#' @param x path for file to be converted
#' @param y path for output file
#' @param gdir directory for png output
#' @param out filename for output file
#' @param gs_cmd passed to \code{\link[tools]{find_gs_cmd}}; perhaps 'gs' or 'gswin32c' or 'mgs' (from Miktex)
#' @param device output device type
#' @param multipage whether to convert multiple pages
#' @param multifix a filename suffix when converting multiple pages
#' @param suffix file extension for output
#' @param antialias font antialiasing
#' @param resolution raster image resolution
#' @param replace whether to delete \code{x} if successful
#' @param other other arguments to ghostscript
#' @param ... ignored
#' @return the name of the file created
#' @examples
#' \dontrun{
#' pdf <- as.pdf(head(Theoph),dir = tempdir())
#' png <- ghostconvert(pdf, gs_cmd = 'mgs')
#' browseURL(png)
#' }
ghostconvert <- function(
  x,
  y = file.path(gdir,out),
  gdir = dirname(x),
  out = sub('\\.[^.]+$',paste0(if(multipage) multifix else NULL, '.',suffix),basename(x)),
  gs_cmd = '',
  device = 'pngalpha',
  multipage = FALSE,
  multifix = '-%03d',
  suffix = 'png',
  antialias = 4,
  resolution = 300,
  replace = TRUE,
  other = '',
  ...

){
  stopifnot(length(x) == 1, length(y) == 1)
  exec <- find_gs_cmd(gs_cmd)
  if(exec == '') stop('gs_cmd not found')
  dev <- paste0('-sDEVICE=',device)
  file <- paste('-o',y)
  alias <- paste0('-dTextAlphaBits=', antialias)
  res <- paste0('-r',resolution)
  command <- paste(exec, dev, file, alias, res, other, x)
  result <- tryCatch(error = function(e) e, system(command))
  bad <- inherits(result, 'try-error') || !file.exists(y)
  if(bad) stop('could not make ', y)
  if(replace) unlink(x)
  invisible(y)
}

#' Coerce to PNG from Data Frame.
#'
#' Coerces to PNG from 'data.frame'.
#' @export
#' @seealso \code{\link{as.pdf.data.frame}}
#' @seealso \code{\link{ghostconvert}}
#' @param x data.frame
#' @param replace whether to delete the intermediate PDF if successful
#' @param ... passed to \code{\link{as.pdf.data.frame}} and to \code{\link{ghostconvert}}
#' @return path to the file created
#' @examples
#' \dontrun{
#' browseURL(as.png(head(Theoph), dir = tempdir(), gs_cmd = 'mgs'))
#' }
as.png.data.frame <- function(x, replace = TRUE, ...){
  result <- as.pdf(x,...)
  ghostconvert(result, replace = replace, ...)
}
