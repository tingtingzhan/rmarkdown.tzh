
#' @title Create R Markdown Script for \link[venn.tzh]{venn}
#' 
#' @description
#' Method dispatch to \link[venn.tzh]{venn} for S3 generic `rmd_` (in a different master package).
#' 
#' @param x a \link[venn.tzh]{venn} object
#' 
#' @param xnm \link[base]{character} scalar, \link[base]{deparse}d call of `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @export rmd_.venn
#' @export
rmd_.venn <- function(x, xnm, ...) {
  # .. is 'gList', but I do not want to hide it (as for 'gDesc')
  # .. does not have ?base::dim
  return(c(
    sprintf(fmt = 'Venn diagram is created using <u>**`R`**</u> package <u>**`VennDiagram`**</u>.'),
    '```{r results = \'asis\'}', 
    sprintf(fmt = 'grid::grid.draw(venn.tzh::zero_venn(%s))', xnm), # my [plot.venn]
    '```'
  ))
}

