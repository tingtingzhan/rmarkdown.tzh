

#' @title R Markdown Script of \link[patchwork]{patchwork}
#' 
#' @param x a \link[patchwork]{patchwork}
#' 
#' @param xnm ..
#' 
#' @param ncol \link[base]{integer} scalar, see function \link[patchwork]{plot_layout}
#' 
#' @param ... ..
#' 
#' @examples
#' id = quote(list(
#'    All = TRUE, Female = (sex == 'Female'), Male = (sex == 'Male')
#' ))
#' library(survival.tzh); list(
#'  '`patchwork`' = subset_ggKM(os ~ ph.ecog, subset = id, data = lung2)
#' ) |> render_(file = 'patchwork')
#' @export rmd_.patchwork
#' @export
rmd_.patchwork <- function(x, xnm, ncol = 2L, ...) {
  
  h_ <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w_ <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  w <- ncol * w_
  h <- ceiling(length(x) / ncol) * h_
  
  return(c(
    sprintf(fmt = '```{r fig.height = %.1f, fig.width = %.1f}', h, w), 
    sprintf(fmt = 'suppressWarnings(%s + patchwork::plot_layout(ncol = %d))', xnm, ncol), # not sure how to put in `...`
    '```',
    '<any-text>'
  ))
  
}


