
rmd_plot_ <- function(x, xnm, ...) {
  h <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    sprintf(fmt = '```{r fig.height = %.1f, fig.width = %.1f}', h, w), 
    sprintf(fmt = '%s |> plot()', xnm),
    # invokes
    # ?lattice:::plot.trellis
    # ?consort:::plot.consort
    # etc.
    '```', 
    '<any-text>'
  ))
}




#' @rdname rmd_
#' @examples
#' library(lattice); Depth = equal.count(quakes$depth, number=8, overlap=.1)
#' library(consort); data(dispos.data)
#' list(
#'  '`trellis`' = xyplot(lat ~ long | Depth, data = quakes),
#'  '`consort`' = consort_plot(
#'   data = dispos.data |> subset(subset = !(arm3 %in% 'Trt C')),
#'   orders = list(c(trialno = 'Population'), c(exclusion = 'Excluded'), c(arm = 'Randomized')),
#'   side_box = c('exclusion'))
#' ) |> render_(file = 'Vanilla Plot')
#' @export rmd_.trellis
#' @export
rmd_.trellis <- rmd_plot_ 

#' @rdname rmd_
#' @export rmd_.consort
#' @export
rmd_.consort <- rmd_plot_


