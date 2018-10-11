#' Escape LaTeX Reserved Characters
#'
#' Escapes LaTeX reserved characters.  Generic, with default, character, and data.frame methods.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family reserve
reserve <- function(x,...)UseMethod('reserve')

#' Escape LaTeX Reserved Characters Using Default Method
#'
#' The default method returns its argument.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family reserve
reserve.default <- function(x,...)x

#' Escape LaTeX Reserved Characters in Lists
#'
#' Applies \code{reserve} to the elements of a list.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family reserve
reserve.list <- function(x,...)lapply(x, reserve, ...)

#' Escape LaTeX Reserved Characters in Data Frames
#'
#' Applies \code{reserve} to the columns of a data.frame.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family reserve
#' @examples
#' foo <- c('#','$%^','&_{','}~\\')
#' bar <- data.frame(
#'  stringsAsFactors = FALSE,
#'  a = as.numeric(factor(foo)),
#'  b = foo,
#'  c = factor(foo)
#' )
#'
#' reserve(bar)
#' # as.pdf(bar, wider = 10)
#' # as.pdf(bar, target = '#') # fails
#'
reserve.data.frame <- function(x,...){
  x[] <- lapply(x, reserve, ...)
  x
}


#' Escape LaTeX Reserved Characters for Character Objects
#'
#' Escapes LaTeX reserved characters as suggested by \url{https://en.wikibooks.org/wiki/LaTeX/Basics#Reserved_Characters}.
#'
#' @param x character
#' @param target sequence to find (regular expression unless fixed is false)
#' @param replacement sequence to use
#' @param ... passed arguments
#' @export
#' @family reserve
#' @export

reserve.character <- function(
  x,
  target      = c('#',   '$',     '%',   '^',     '&',   '_',   '{',   '}',   '~',   '\\'),
  replacement = c('\\#', '\\$', '\\%', '\\^{}', '\\&', '\\_', '\\{', '\\}', '\\~{}', '\\textbackslash{}'),
  ...
) {
  stopifnot(is.character(target), is.character(replacement), length(target) == length(replacement))
  x <- strsplit(x,'')
  for(i in seq_along(x)){
    for(j in seq_along(x[[i]])){
      c <- match(x[[i]][[j]], target)
      if(!is.na(c)) x[[i]][[j]] <- replacement[[c]]
    }
    x[[i]] <- paste(x[[i]], collapse = '')
  }
  x <- unlist(x)
  x
}

#' Escape LaTeX Reserved Characters for Factor Objects
#'
#' Coerces to character and escapes reserved characters.
#'
#' @param x factor
#' @param ... passed arguments
#' @export
#' @family reserve
#' @export
reserve.factor <- function(x, ...)reserve(as.character(x,...),...)

