

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
#' set.seed(100)
#' x = matrix(rnorm(50), ncol = 5, dimnames = list(NULL, LETTERS[1:5]))
#' y = matrix(rnorm(30), ncol = 3, dimnames = list(NULL, letters[1:3]))
#' library(htest.tzh); list(
#'   '`htest_array`' = outer.cor.test(x, y)
#' ) |> render_(file = 'htest_array')
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
#' @importFrom htest.tzh rmd_.htest_array
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
    '```{r}',
    paste0('as_flextable_dataframe(', xnm, ')'),
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
    '```{r}',
    paste0('as_flextable.array(', xnm, ')'), # 3-dimension not working well now!!
    '```', 
    '<any-text>'
  ))
}








#' @rdname rmd_
#' @export rmd_.list
#' @export
rmd_.list <- function(x, xnm, ...) {
  tmp <- lapply(seq_along(x), FUN = function(i) {
    c(rmd_(x = x[[i]], xnm = paste0(xnm, '[[', i, ']]'), ...), '\n\n')
  })
  unlist(tmp, recursive = FALSE, use.names = FALSE)
}

#' @rdname rmd_
#' @export rmd_.numeric
#' @export
rmd_.numeric <- function(x, ...) {
  paste(x, collapse = ', ')
}

#' @rdname rmd_
#' @export rmd_.character
#' @export
rmd_.character <- function(x, ...) x # not ?base::identity




#' @rdname rmd_
#' @export rmd_.noquote
#' @export
rmd_.noquote <- function(x, xnm, ...) {
  rmd_(x = unclass(x), xnm = sprintf(fmt = 'unclass(%s)', xnm), ...)
}









#' @rdname rmd_
#' @examples
#' library(ggplot2); list(
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
  #    '```{r}', 
  #    if (!is.recursive(corr)) {
  #      sprintf(fmt = 'as_flextable(attr(%s, which = \'corr\', exact = TRUE))', xnm)
  #    } else sprintf(fmt = 'as_flextable(attr(%s, which = \'corr\', exact = TRUE)[[%d]])', xnm, seq_along(corr)), 
  #    '```'
  #  )
  #} # else NULL
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    sprintf(fmt = '```{r fig.height = %.1f, fig.width = %.1f}', h, w), 
    sprintf(fmt = 'suppressWarnings(print(%s))', xnm), 
    '```',
    # corr_rmd,
    '<any-text>'
  ))
}


#' @rdname rmd_
#' @examples
#' library(plotly); list(
#'  '`htmlwidget` 1' = plot_ly(economics, x = ~date, y = ~pop, type = 'scatter', mode = 'markers'),
#'  '`htmlwidget` 2' = plot_ly(z = ~volcano, type = "surface")
#' ) |> render_(file = 'htmlwidget')
#' @export rmd_.htmlwidget
#' @export
rmd_.htmlwidget <- function(x, xnm, ...) {
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    '```{r}',
    xnm, # invokes ?htmlwidgets:::print.htmlwidget
    # sprintf(fmt = '%s |> print()', xnm), # do *not* do this!!
    '```',
    '<any-text>'
  ))
  
}






#' @rdname rmd_
#' @export
rmd_.gDesc <- function(x, xnm, ...) {
  .Defunct(msg = 'use library(patchwork) instead!')
  dm <- dim(x) # gtable:::dim.gtable
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    if (length(dm) == 2L) {
      sprintf(fmt = '```{r fig.height = %.1f, fig.width = %.1f}', 4*dm[1L], 6*dm[2L])
    } else '```{r}', 
    sprintf(fmt = 'grid::grid.draw(%s)', xnm), 
    '```'
  ))
}

#' @rdname rmd_
#' @export
rmd_.gList <- rmd_.gDesc




#' @rdname rmd_
#' @examples
#' library(flextable); list(
#'  '`flextable`' = flextable(mtcars)
#' ) |> render_(file = 'flextable')
#' @export rmd_.flextable
#' @export
rmd_.flextable <- function(x, xnm, ...) {
  return(c(
    '```{r}',
    xnm,
    '```'
  ))
}







