# Quoting from http://en.wikibooks.org/wiki/LaTeX/Absolute_Beginners ...
# "Anything in LaTeX can be expressed in terms of commands and environments."

# LaTeX Commands
# LaTeX commands are case sensitive, and take one of the following two formats:
#  - They start with a backslash \ and then have a name consisting of letters only. Command names are terminated by a space, a number or any #other "non-letter".
#  - They consist of a backslash \ and exactly one non-letter.
# Some commands need an argument, which has to be given between curly braces { } after the command name. Some commands support optional #parameters, which are added after the command name in square brackets [ ]. The general syntax is:
# \commandname[option1,option2,...]{argument1}{argument2}...

# LaTeX environments
# Environments in LaTeX have a role that is quite similar to commands, but they usually have effect on a wider part of the document. Their syntax #is:
# \begin{environmentname}
# text to be influenced
# \end{environmentname}
# Between the \begin and the \end you can put other commands and nested environments. In general, environments can accept arguments as well, but #this feature is not commonly used and so it will be discussed in more advanced parts of the document.

is.alpha <- function(x,...){
  stopifnot(is.character(x))
  font <- c(letters,LETTERS)
  x <- strsplit(x,'')
  sapply(x,function(vec)all(vec %in% font))
}
is.one.nonalpha <- function(x,...){
  stopifnot(is.character(x))
  font <- c(letters,LETTERS)
  (nchar(x)==1) & (!x %in% font)
}
is.latex.token <- function(x,...)is.alpha(x) | is.one.nonalpha(x)
spaces <- function(x)paste(rep(' ',x),collapse='')
tagvalue <- function(x,sep='=',collapse=',',...){
  stopifnot(is.list(x))
  y <- sapply(
    seq_along(x),
    function(num){
      tag <- names(x)[[num]]
      value <- x[[num]]
      if(!is.null(tag))
        if(!is.na(tag))
          if(tag != "")value <- paste(tag,value,sep=sep)
      value
    }
  )
  paste(y,collapse=collapse)
}
latex.options <- function(x,...){
  x <- as.list(x)
  x <- tagvalue(x)
  if(nchar(x)) x <- paste0('[',x,']')
  x
}
latex.args <- function(x,...){
  x <- sapply(x,function(x)paste0('{',x,'}'))
  x <- paste(x,collapse='')
  x
}

#' Calculate Row and Column Breaks
#'
#' Calculates row and column breaks by finding first elements among repeats.
#' @export
#' @seealso \code{\link{as.tabular}}
#' @param x vector
#' @param ... ignored
#' @return integer
breaks <- function(x,...)as.integer(runhead(x))[-1]

runhead <- function (x) {
  n <- x != prev(x)
  if (length(n))
    n[[1]] <- TRUE
  n
}
tabularformat <- function(justify,breaks,walls){
  stopifnot(
    length(walls)==2,
    length(justify)==length(breaks)+1,
    is.numeric(breaks),
    is.numeric(walls),
    !any(is.na(breaks)),
    !any(is.na(walls))
  )
  format <- ''
  format <- append(format, rep('|',walls[[1]]))
  for(i in seq_along(breaks)){
    format <- append(format,justify[[i]])
    format <- append(format,rep('|',breaks[[i]]))
  }
  #since there is one more justify than breaks ...
  format <- append(format, rev(justify)[[1]])
  format <- append(format, rep('|',walls[[2]]))
  format <- paste(format,collapse='')
  format
}
row2tabular <- function(x,...){
  x <- paste(x,collapse=' & ')
  x <- paste(x,'\\\\')
  x
}
align.decimal <- function(x,decimal.mark='.',...){
  x <- prettyNum(x)
  nodecimal <- !contains(decimal.mark,x,fixed=TRUE)
  x[nodecimal] <- paste0(x[nodecimal],' ')
  splits <- strsplit(x,decimal.mark,fixed=TRUE)
  splits <- lapply(splits,function(x){if(length(x)==1)x[[2]]<-'';x})
  tails <- sapply(splits,function(x)nchar(x[[2]]))
  need <- max(tails) - tails
  while(any(need > 0)){
    x[need > 0] <- paste0(x[need > 0],' ')
    need <- need - 1
  }
  x
}
padded <- function (x, width = 4, ...) sprintf(paste0("%0",width,".0f"), x)

#' Format a Latex Command
#'
#' Format a latex command.  \code{x} is formated as a latex command, with the options (possibly named) inserted in square brackets, and the arguments each enclosed in curly braces.  \code{depth} spaces are added to the left end of the string.
#' @param x length one character
#' @param options vector or list
#' @param args vector or list
#' @param depth integer
#' @return character
#' @seealso \code{\link{wrap}}
#' @seealso \code{\link{as.ltable.data.frame}}
#' @export
#' @examples
#' command('caption',options='short',args='long')
#'
command <- function(x, options=NULL, args=NULL,depth=0){
  stopifnot(length(x)==1, is.latex.token(x))
  options <- latex.options(options)
  args <- latex.args(args)
  res <- paste(c(spaces(depth),'\\',x,options,args),collapse='')
  res
}

#' Wrap Text in a Latex Environment
#'
#' Wrap text in a latex environment. \code{x} is wrapped in the specified environment, with options and arguments formatted appropriately.
#'
#' @param x character
#' @param environment name of environment
#' @param options list or vector
#' @param args list or vector
#' @param depth integer (extra spaces on the left)
#' @return character
#' @seealso \code{\link{command}}
#' @seealso \code{\link{as.ltable.data.frame}}
#' @export
#' @examples
#' wrap('Hello','center')

wrap <- function(x,environment,options=NULL,args=NULL,depth=0){
  stopifnot(is.character(x),length(environment)==1,is.latex.token(environment))
  options <- latex.options(options)
  args <- latex.args(args)
  begin <- paste0(spaces(depth),'\\begin{',environment,'}',options,args)
  end   <- paste0(spaces(depth),'\\end{',environment,'}')
  x <- paste0(spaces(depth+1),x)
  res <- c(begin,x,end)
  res
}

