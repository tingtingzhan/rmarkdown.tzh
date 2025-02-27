

# '<any-text>'
# 'word_document' needs *more* than '\n' to correctly separate two flextable's.  
# 'html_document' needs nothing, and will take '<>' as meaningless html tag (thus ignored) :)


#' @title Generate R Markdown Script
#' 
#' @param x an R object
#' 
#' @param xnm ..
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @name rmd_
#' @export
rmd_ <- function(x, ...) {
  if (!length(x)) return(invisible())
  UseMethod('rmd_')
}


#' @rdname rmd_
#' @export rmd_.default
#' @export
rmd_.default <- function(x, xnm, ...) {
  # currently accepts
  # 'power.htest'
  # 'htest'
  return(c(
    '```{r comment = NA}', 
    xnm,
    '```'
  ))
}




#' @rdname rmd_
#' @export
rmd_.list <- function(x, xnm, ...) {
  tmp <- lapply(seq_along(x), FUN = function(i) {
    c(rmd_(x = x[[i]], xnm = paste0(xnm, '[[', i, ']]'), ...), '\n\n')
  })
  unlist(tmp, recursive = FALSE, use.names = FALSE)
}

#' @rdname rmd_
#' @export
rmd_.numeric <- function(x, ...) {
  paste(x, collapse = ', ')
}

#' @rdname rmd_
#' @export
rmd_.character <- function(x, ...) x # not ?base::identity




#' @rdname rmd_
#' @export
rmd_.noquote <- function(x, xnm, ...) {
  rmd_(x = unclass(x), xnm = sprintf(fmt = 'unclass(%s)', xnm), ...)
}









#' @rdname rmd_
#' @export
rmd_.ggplot <- function(x, xnm, ...) {
  
  h <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  #corr <- attr(x, which = 'corr', exact = TRUE) # correlation coefficients from [ggScatter]
  #corr_rmd <- if (length(corr)) {
  #  c(
  #    '```{r results=\'asis\'}', 
  #    if (!is.recursive(corr)) {
  #      sprintf(fmt = 'as_flextable(attr(%s, which = \'corr\', exact = TRUE))', xnm)
  #    } else sprintf(fmt = 'as_flextable(attr(%s, which = \'corr\', exact = TRUE)[[%d]])', xnm, seq_along(corr)), 
  #    '```'
  #  )
  #} # else NULL
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    sprintf(fmt = '```{r results = \'asis\', fig.height = %.1f, fig.width = %.1f}', h, w), 
    sprintf(fmt = 'suppressWarnings(print(%s))', xnm), 
    '```',
    # corr_rmd,
    '<any-text>'
  ))
}






#' @rdname rmd_
#' @export
rmd_.gDesc <- function(x, xnm, ...) {
  dm <- dim(x) # gtable:::dim.gtable
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    if (length(dm) == 2L) {
      sprintf(fmt = '```{r results = \'asis\', fig.height = %.1f, fig.width = %.1f}', 4*dm[1L], 6*dm[2L])
    } else '```{r results = \'asis\'}', 
    sprintf(fmt = 'grid::grid.draw(%s)', xnm), 
    '```'
  ))
}

#' @rdname rmd_
#' @export
rmd_.gList <- rmd_.gDesc




#' @rdname rmd_
#' @export
rmd_.flextable <- function(x, xnm, ...) {
  return(c(
    '```{r results = \'asis\'}', 
    xnm,
    '```'
  ))
}







