

# '<any-text>'
# 'word_document' needs *more* than '\n' to correctly separate two flextable's.  
# 'html_document' needs nothing, and will take '<>' as meaningless html tag (thus ignored) :)


#' @title R Markdown Script
#' 
#' @description
#' To create R markdown script for various objects.
#' 
#' @param x see **Usage**
#' 
#' @param xnm ..
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [rmd_()] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @examples
#' library(mDFR)
#' ds = split(santos1, f = ~ Hr + antigen)
#' list(
#'  '`maxT`' = maxT_santos_test(data1 = ds$`18.CEF`, data0 = ds$`0.CEF`)
#' ) |> render_(file = 'maxT')
#' 
#' set.seed(100)
#' x = matrix(rnorm(50), ncol = 5, dimnames = list(NULL, LETTERS[1:5]))
#' y = matrix(rnorm(30), ncol = 3, dimnames = list(NULL, letters[1:3]))
#' library(htest.tzh); list(
#'  '`htest_array`' = outer.cor.test(x, y)
#' ) |> render_(file = 'htest_array')
#' 
#' library(rpart); list(
#'  '`rpart`' = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, model = TRUE)
#' ) |> render_(file = 'rpart')
#' 
#' library(psych); list(
#'  'fa' = swiss |> cov() |> fa(nfactors = 2L, fm = 'pa', rotate = 'varimax')
#' ) |> render_(file = 'fa')
#' 
#' 
#' @name rmd_
#' @importFrom htest.tzh rmd_.htest_array
#' @importFrom mDFR rmd_.maxT
#' @importFrom psych.tzh rmd_.fa plot_fa_
#' @importFrom rpart.tzh rmd_.rpart prp_
#' @export
rmd_ <- function(x, ...) {
  if (!length(x)) return(invisible())
  UseMethod(generic = 'rmd_')
}



#' @rdname rmd_
#' @examples
#' list('`data.frame`' = swiss) |> render_(file = 'data.frame')
#' 
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
#' list('`matrix`' = VADeaths) |> render_(file = 'matrix')
#' 
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
  x |> 
    seq_along() |>
    lapply(FUN = \(i) {
      c(rmd_(x = x[[i]], xnm = paste0(xnm, '[[', i, ']]'), ...), '\n\n')
    }) |> 
    unlist(recursive = FALSE, use.names = FALSE)
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



