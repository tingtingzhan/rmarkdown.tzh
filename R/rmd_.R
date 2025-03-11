

# '<any-text>'
# 'word_document' needs *more* than '\n' to correctly separate two flextable's.  
# 'html_document' needs nothing, and will take '<>' as meaningless html tag (thus ignored) :)


#' @title R Markdown Script
#' 
#' @param x an R object
#' 
#' @param xnm ..
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @examples
#' \donttest{
#' library(mDFR)
#' ?maxT_santos_test
#' ds = split(santos1, f = ~ Hr + antigen)
#' list(
#'  '`maxT`' = maxT_santos_test(data1 = ds$`18.CEF`, data0 = ds$`0.CEF`)
#' ) |> render_(file = 'maxT')
#' } # slow
#' 
#' library(rpart.tzh); library(rpart); list(
#'   '`rpart`' = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, model = TRUE)
#' ) |> render_(file = 'rpart')
#' 
#' library(venn.tzh); list(
#'   '`venn`' = venn(list(A = state.name[1:20], B = state.name[15:30]))
#' ) |> render_(file = 'venn')
#' 
#' library(consort.tzh); list(
#'   '`consort`' = consort_plot(data = dispos.data, 
#'     orders = c(trialno = 'Population', exclusion = 'Excluded', trialno = 'Allocated'),
#'     side_box = c('exclusion'), cex = 0.9)
#' ) |> render_(file = 'consort')
#' @name rmd_
#' @importFrom consort.tzh rmd_.consort
#' @importFrom mDFR rmd_.maxT
#' @importFrom rpart.tzh rmd_.rpart
#' @importFrom venn.tzh rmd_.venn
#' @export
rmd_ <- function(x, ...) {
  if (!length(x)) return(invisible())
  UseMethod(generic = 'rmd_')
}

rmd_naive <- function(x, xnm, ...) {
  return(c(
    '```{r comment = NA}', 
    xnm,
    '```'
  ))
}

#' @rdname rmd_
#' @export rmd_.htest
#' @export
rmd_.htest <- rmd_naive


#' @rdname rmd_
#' @examples
#' list(
#'   '`htest`' = list(
#'    t.test(mpg ~ am, data = mtcars)
#'   ),
#'   '`power.htest`' = list(
#'    power.t.test(power = .90, delta = 1)
#'   )
#' ) |> render_(file = 'htest')
#' @method rmd_ power.htest
#' @export rmd_.power.htest
#' @export
rmd_.power.htest <- rmd_naive





#' @rdname rmd_
#' @examples
#' list(
#'   '`data.frame`' = swiss
#' ) |> render_(file = 'data.frame')
#' @export rmd_.data.frame
#' @export
rmd_.data.frame <- function(x, xnm, ...) {
  return(c(
    '```{r results = \'asis\'}', 
    paste0('flextable.tzh::as_flextable_dataframe(', xnm, ')'),
    '```', 
    '<any-text>'
  ))
}


#' @rdname rmd_
#' @examples
#' list(
#'   '`matrix`' = VADeaths
#' ) |> render_(file = 'matrix')
#' @export rmd_.array
#' @export
rmd_.array <- function(x, xnm, ...) {
  return(c(
    '```{r results = \'asis\'}', 
    paste0('flextable.tzh::as_flextable.array(', xnm, ')'), # 3-dimension not working well now!!
    '```', 
    '<any-text>'
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
#' @examples
#' library(ggplot2)
#' list(
#'   '`ggplot`' = ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' ) |> render_(file = 'ggplot')
#' @export rmd_.ggplot
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







