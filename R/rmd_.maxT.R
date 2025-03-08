

#' @title rmd_.maxT
#' 
#' @description
#' ..
#' 
#' @param x \link[mDFR]{maxT-class} object
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' \donttest{
#' library(mDFR)
#' ?maxT_santos_test
#' ds = split(santos1, f = ~ Hr + antigen)
#' list(
#'  '`maxT`' = maxT_santos_test(data1 = ds$`18.CEF`, data0 = ds$`0.CEF`)
#' ) |> render_(file = 'maxT')
#' }
#' @export rmd_.maxT
#' @export
rmd_.maxT <- function(x, xnm, ...) c(
  '```{r results = \'asis\'}', 
  sprintf(fmt = 'reactable_maxT(%s)', xnm),
  '```', 
  sprintf(fmt = '```{r results = \'asis\', fig.height = %.1f, fig.width = %.1f}', 7, 6), 
  sprintf(fmt = 'suppressWarnings(grid::grid.draw(%s@gtable))', xnm), 
  # ?grid:::grid.draw.gTree; 
  # workhorse ?grid:::drawGTree
  # this step is the slowest for big data
  '```',
  '<any-text>'
)



