
#' @rdname rmd_
#' @examples
#' library(lattice)
#' Depth = equal.count(quakes$depth, number=8, overlap=.1)
#' list(
#'  '`trellis`' = xyplot(lat ~ long | Depth, data = quakes)
#' ) |> render_(file = 'trellis')
#' @export rmd_.trellis
#' @export
rmd_.trellis <- function(x, xnm, ...) {
  return(c(
    '```{r}',
    paste0('plot(', xnm, ')'), # ?lattice:::plot.trellis
    '```', 
    '<any-text>'
  ))
}