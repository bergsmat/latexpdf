map <-
  function (x, from, to, strict = TRUE, ...)
  {
    stopifnot(length(to) == length(from))
    res <- to[match(x, table = from)]
    if (!strict)
      res[!(x %in% from)] <- x[!(x %in% from)]
    res
  }
contains <-
  function (pattern, text, ...)
  {
    hits <- regexpr(pattern, text, ...)
    hits >= 0
  }
prev <-
  function (x)
  {
    s <- seq_along(x)
    s <- c(length(s), s[-length(s)])
    x <- x[s]
    if (length(x))
      x[[1]] <- NA
    x
  }
shuffle <-
  function (x, who, after = NA)
  {
    names(x) <- make.unique(names(x))
    who <- names(x[, who, drop = FALSE])
    nms <- names(x)[!names(x) %in% who]
    if (is.null(after))
      after <- length(nms)
    if (is.na(after))
      after <- 0
    if (length(after) == 0)
      after <- length(nms)
    if (is.character(after))
      after <- match(after, nms, nomatch = 0)
    if (after < 0)
      after <- length(nms)
    if (after > length(nms))
      after <- length(nms)
    nms <- append(nms, who, after = after)
    x[nms]
  }
reapply <-
  function (x, INDEX, FUN, where, ...)
  {
    if (missing(where))
      where <- rep(TRUE, length(x))
    where <- rep(where, length.out = length(x))
    where[is.na(where)] <- TRUE
    where <- factor(where, levels = c(FALSE, TRUE))
    if (!is.list(INDEX))
      INDEX <- list(INDEX)
    INDEX <- do.call(interaction, c(INDEX, list(drop = TRUE)))
    WINDEX <- interaction(where, INDEX, drop = FALSE)
    wvals <- split(x, WINDEX)
    vals <- wvals[c(FALSE, TRUE)]
    stopifnot(identical(length(vals), length(levels(INDEX))))
    val <- lapply(vals, FUN, ...)
    dim <- table(INDEX)
    val <- lapply(seq_along(val), function(i) rep(val[[i]], length.out = dim[[i]]))
    t <- NA
    if (length(val))
      if (length(val[[1]]))
        t <- val[[1]][[1]]
    t[[1]] <- NA
    y <- rep(t[[1]], length(x))
    split(y, INDEX) <- val
    y[is.na(INDEX)] <- NA
    y
  }
last <- function (x, where, within, ...) nth(x = x, where = where, within = within, n = -1)
nth <-
  function (x, where, within, n = 1, ...)
  {
    scale <- 0
    n <- as.integer(n)
    stopifnot(length(n) == 1)
    if (!missing(x))
      scale <- max(scale, length(x))
    if (!missing(where)) {
      where <- as.logical(where)
      scale <- max(scale, length(where))
    }
    if (!missing(within)) {
      if (is.list(within))
        within <- as.numeric(factor(do.call(paste, within)))
      scale <- max(scale, length(within))
    }
    if (!missing(x))
      if (length(x))
        if (scale%%length(x) != 0)
          warning("scale not a multiple of x")
    if (!missing(where))
      if (length(where))
        if (scale%%length(where) != 0)
          warning("scale not a multiple of where")
    if (!missing(within))
      if (length(within))
        if (scale%%length(within) != 0)
          warning("scale not a multiple of within")
    if (missing(x))
      x <- seq(length.out = scale)
    if (missing(where))
      where <- rep(TRUE, scale)
    if (missing(within))
      within <- rep(TRUE, scale)
    x <- rep(x, length.out = scale)
    where <- rep(where, length.out = scale)
    within <- rep(within, length.out = scale)
    actual <- paste(within, where)
    ideal <- paste(within, rep(TRUE, scale))
    if (is.na(n))
      return(x)
    if (n == 1)
      return(x[match(ideal, actual)])
    if (n == 0)
      return(rep(NA, scale))
    if (n < 0)
      return(rev(nth(x = rev(x), where = rev(where), within = rev(within),
                     n = -n)))
    where[unique(nth(where = where, within = within, n = 1))] <- FALSE
    nth(x = x, where = where, within = within, n = n - 1)
  }
maxChar <-function (x){
    if(!length(x)) return(0)
    x <- as.character(x)
    len <- nchar(x)
    max(len)
}
fixedwidth.data.frame <-
function (x, ...)
{
    x <- data.frame(lapply(x, format, justify = "right"), stringsAsFactors = FALSE)
    x <- rbind(names(x), x)
    x <- data.frame(lapply(x, format, justify = "right"), stringsAsFactors = FALSE)
    names(x) <- x[1, ]
    x[-1, , drop = FALSE]
}

