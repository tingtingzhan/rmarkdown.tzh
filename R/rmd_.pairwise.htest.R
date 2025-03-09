

#' @title R Markdown Script of `pairwise.htest`
#' 
#' @description ..
#' 
#' @param x a `pairwise.htest` object, i.e., the returned value from functions 
#' \link[stats]{pairwise.t.test}, \link[stats]{pairwise.wilcox.test} 
#' or \link[stats]{pairwise.prop.test}
#' 
#' @param xnm \link[base]{character} scalar, call of `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [rmd_.pairwise.htest] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' m = airquality |> 
#'   within.data.frame(expr = {
#'     Month = factor(Month, labels = month.abb[5:9])
#'   }) |>
#'   with(expr = pairwise.t.test(Ozone, Month, pool.sd = FALSE, p.adj = 'none'))
#' list(
#'   '`pairwise.htest`' = m
#' ) |> render_(file = 'pairwise.htest')
#' @export rmd_.pairwise.htest
#' @export
rmd_.pairwise.htest <- function(x, xnm, ...) c(
  '```{r results = \'asis\'}', 
  sprintf(fmt = 'as_flextable.pairwise.htest(%s)', xnm), 
  sprintf(fmt = 'as_flextable.array(format_pval(p_adjust_.pairwise.htest(%s)))', xnm), 
  '```', 
  '<any-text>'
)

