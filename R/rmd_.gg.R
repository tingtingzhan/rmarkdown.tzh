
#' @rdname rmd_
#' @examples
#' library(ggplot2); list(
#'   '`ggplot2::ggplot`' = ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'   '`GGally::ggmatrix`' = GGally::ggpairs(swiss, columns = c(1:2, 6))
#' ) |> render_(file = 'gg')
#' @export rmd_.gg
#' @export
rmd_.gg <- function(x, xnm, ...) {
  
  h <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    sprintf(fmt = '```{r fig.height = %.1f, fig.width = %.1f}', h, w), 
    sprintf(fmt = 'suppressWarnings(print(%s))', xnm), 
    # invokes
    # ?ggplot2:::print.ggplot
    # ?GGally:::print.ggmatrix
    # etc.
    '```',
    '<any-text>'
  ))
}
