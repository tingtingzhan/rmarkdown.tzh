
# packageDate('patchwork')
# packageDate('ggpubr')



#' @rdname rmd_
#' 
#' @param ncol \link[base]{integer} scalar for function [rmd_.patchwork()], see function \link[patchwork]{plot_layout}
#' 
#' @note
#' As for now (early 2025), tzh prefers package \pkg{patchwork} over 
#' function `ggpubr::ggarrange`.
#' 
#' @examples
#' id = quote(list(
#'    All = TRUE, Female = (sex == 'Female'), Male = (sex == 'Male')
#' ))
#' library(survival.tzh); list(
#'  '`patchwork`' = subset_ggKM(os ~ ph.ecog, subset = id, data = lung2)
#' ) |> render_(file = 'patchwork')
#' 
#' @export rmd_.patchwork
#' @export
rmd_.patchwork <- function(x, xnm, ncol = 2L, ...) {
  
  h_ <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w_ <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  w <- ncol * w_
  h <- ceiling(length(x) / ncol) * h_
  
  return(c(
    sprintf(fmt = '```{r fig.height = %.1f, fig.width = %.1f}', h, w), 
    # '```{r}', # automatic height and weight not good; packageDate('patchwork') 2024-09-16
    sprintf(fmt = 'suppressWarnings(%s + patchwork::plot_layout(ncol = %d))', xnm, ncol), # not sure how to put in `...`
    '```',
    '<any-text>'
  ))
  
}


