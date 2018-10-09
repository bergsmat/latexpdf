

## =================================================
## 2018-10-08 PACKAGE latexpdf
## dfr(utf8) -> protectTex -> latexpdf -> tex -> pdf
## =================================================





#' @title Protect the LaTeX reserved characters
#' @description
#' Protect the LaTeX reserved characters with antislash(es) \code{\\} so that the 
#' output can be read by LaTeX compiler.

## The following function accepts (list of) character vector or data.frame.
## I had to use classical loops since apply(x, 2, fun) returns strange results.
## The code is a simplified version of pixiedust::sanitize_latex and does not 
## contain any option for Greek letters. xtable has its own function.

#' @param   x       (list of) character, vector of characters, data.frame of characters.
#' @examples 
#' protectTex(c("a", "$", "cde", "{", "fgh_uv"))
#' @export
#' @name protectTex
protectTex <- function(x) {
    if (is.list(x) && !is.data.frame(x)) {
        z <- lapply(x, protectTex)
        names(z) <- names(x)
    } else {
        protectChar <- function(char) {
            if (char %in% c("#", "$", "%", "^", "&", "_", "{", "}", "~", "\\")) {
                paste0("\\", char)
            } else { char }
        }
        if (is.null(dim(x))) {
            z <- x
            for (i in seq_along(x)) {
                vec  <- unlist(strsplit(x[i], ""))
                z[i] <- paste(sapply(vec, protectChar), collapse = "")
            }
        } else {
            z <- x
            for (i in 1:NROW(x)) {
                for (j in 1:NCOL(x)) {
                    vec    <- unlist(strsplit(x[i,j], ""))
                    z[i,j] <- paste(sapply(vec, protectChar), collapse = "")
                }
            }
        }
    }
return(z)
}




## If using Latex engine (but not Xe(la)tex our Lua(la)tex engines)
## adding inputenc and fontenc instructions is recommanded.
## For a 3 columns table, here is my code


## VERSION FILE + AS.DOCUMENT
filename <- "latexpdf-asdoc.tex"   ; filename
con      <- file(filename, open = "wt", encoding = "UTF-8") ; con
writeLines(
as.document(protectTex(x),
            preamble = makePreamble(
             wide="297", long="210", geoLeft="10mm", 
             geoTop="10mm", geoRight="10mm", geoBottom="10mm", 
             morePreamble = c(
              command("usepackage", options="utf8", args="inputenc"),
              command("usepackage", options="T1", args="fontenc")
            )),
            rules = c(1,1), walls = c(1,1), 
            grid = TRUE, colwidth = paste0(c(35,70,135), "mm"),
            tabularEnvironment = "longtable"
            ), 
con)
close(con)
tools::texi2pdf(filename, clean = TRUE)


## VERSION WRITELINES + LTABLE
filename <- "latexpdf-wLines.tex"   ; filename
con      <- file(filename, open = "wt", encoding = "UTF-8") ; con
writeLines(c(
    command("documentclass", options="a4paper,landscape,10pt", args="article"),
    command("usepackage", options="margin=10mm", args="geometry"),
    command("usepackage", options="utf8", args="inputenc"),
    command("usepackage", options="T1", args="fontenc"),
    command("usepackage", args="longtable"),
    command("begin", args="document"),
    command("thispagestyle", args="empty"),
    command("pagestyle", args="empty"),
    as.tabular(protectTex(x),  
              rules = c(1,1), walls = c(1,1), 
              grid = TRUE, colwidth = paste0(c(35,70,135), "mm"),
              tabularEnvironment = "longtable"), 
    command("end", args="document")),
con)
close(con)
tools::texi2pdf(filename, clean = TRUE)
#

