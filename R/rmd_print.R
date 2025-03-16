

rmd_print0_ <- function(x, xnm, ...) {
  h <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    sprintf(fmt = '```{r comment = NA, fig.height = %.1f, fig.width = %.1f}', h, w), 
    xnm, # print, but not say print
    # invokes
    # ?flextable:::print.flextable
    # ?htmlwidgets:::print.htmlwidget
    # etc.
    '```',
    '<any-text>'
  ))
}


rmd_print_ <- function(x, xnm, ...) {
  h <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  return(c(
    attr(x, which = 'text', exact = TRUE),
    '\n',
    sprintf(fmt = '```{r comment = NA, fig.height = %.1f, fig.width = %.1f}', h, w), 
    sprintf(fmt = '%s |> print() |> suppressWarnings()', xnm),
    # invokes
    # ?stats:::print.htest
    # ?stats:::print.power.htest
    # ?ggplot2:::print.ggplot
    # ?GGally:::print.ggmatrix
    # etc.
    '```',
    '<any-text>'
  ))
}


#' @rdname rmd_
#' @examples
#' library(ggplot2); list(
#'   '`htest`' = t.test(mpg ~ am, data = mtcars),
#'   '`power.htest`' = power.t.test(power = .90, delta = 1),
#'   '`ggplot2::ggplot`' = ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'   '`GGally::ggmatrix`' = GGally::ggpairs(swiss, columns = c(1:2, 6))
#' ) |> render_(file = 'Explicit Print')
#' @export rmd_.gg
#' @export
rmd_.gg <- rmd_print_

#' @rdname rmd_
#' @export rmd_.htest
#' @export
rmd_.htest <- rmd_print_ # rmd_print0; either okay

#' @rdname rmd_
#' @method rmd_ power.htest
#' @export rmd_.power.htest
#' @export
rmd_.power.htest <- rmd_print_ # rmd_print0; either okay


#' @rdname rmd_
#' 
#' @note
#' Read \url{https://plotly.com/r/subplots/} 
#' on how to stack `'plotly'` objects 
#' (via function \link[plotly]{subplot}).
#' 
#' @examples
#' library(flextable); library(reactable); library(plotly); 
#' list(
#'  '`flextable::flextable`' = flextable(mtcars),
#'  '`reactable::reactable`, inherits from `htmlwidget`' = reactable(iris),
#'  '`htmlwidget`' = list(
#'    plot_ly(economics, x = ~date, y = ~pop, type = 'scatter', mode = 'markers'),
#'    plot_ly(z = ~volcano, type = "surface")
#'  )
#' ) |> render_(file = 'Do Not Say Print')
#' @export rmd_.flextable
#' @export
rmd_.flextable <- rmd_print0_ # rmd_print_ *not* okay!!

#' @rdname rmd_
#' @export rmd_.htmlwidget
#' @export
rmd_.htmlwidget <- rmd_print0_ # rmd_print_ *not* okay!!

