

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




