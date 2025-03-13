
rmd_naive <- function(x, xnm, ...) {
  return(c(
    '```{r comment = NA}', 
    xnm,
    '```'
  ))
}

#' @rdname rmd_
#' @export rmd_.htest
#' @export
rmd_.htest <- rmd_naive

#' @rdname rmd_
#' @examples
#' list(
#'   '`htest`' = list(
#'    t.test(mpg ~ am, data = mtcars)
#'   ),
#'   '`power.htest`' = list(
#'    power.t.test(power = .90, delta = 1)
#'   )
#' ) |> render_(file = 'htest')
#' @method rmd_ power.htest
#' @export rmd_.power.htest
#' @export
rmd_.power.htest <- rmd_naive


