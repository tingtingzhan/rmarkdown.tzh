

#' @rdname rmd_
#' 
#' @examples
#' list(
#'   '`pairwise.htest`' = airquality |> 
#'     within.data.frame(expr = {
#'       Month = factor(Month, labels = month.abb[5:9])
#'     }) |>
#'     with(expr = pairwise.t.test(Ozone, Month, pool.sd = FALSE, p.adj = 'none'))
#' ) |> render_(file = 'pairwise.htest')
#' 
#' @export rmd_.pairwise.htest
#' @export
rmd_.pairwise.htest <- function(x, xnm, ...) c(
  '```{r}', # multiple ?flextable::flextable
  sprintf(fmt = '(%s) |> as_flextable.pairwise.htest()', xnm), 
  sprintf(fmt = '(%s) |> p_adjust_.pairwise.htest() |> format_pval() |> as_flextable.array()', xnm), 
  '```', 
  '<any-text>'
)

