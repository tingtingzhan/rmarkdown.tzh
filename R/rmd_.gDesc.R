
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

