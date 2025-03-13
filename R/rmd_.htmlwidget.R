

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
