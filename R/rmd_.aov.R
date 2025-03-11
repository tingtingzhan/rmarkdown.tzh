



#' @rdname rmd_
#' @examples
#' m = aov(breaks ~ wool + tension, data = warpbreaks)
#' list(
#'   '`aov`' = m,
#'   '`TukeyHSD`' = m |> TukeyHSD(which = 'tension', ordered = TRUE)
#' ) |> render_(file = 'aov_etc')
#' @export rmd_.aov
#' @export
rmd_.aov <- function(x, xnm, ...) {
  return(c(
    'Analysis of variance is performed using <u>**`R`**</u>.',
    '```{r results=\'asis\'}', 
    sprintf(fmt = 'flextable.tzh::as_flextable.aov(%s)', xnm),
    '```', 
    '<any-text>'
  ))
}




#' @rdname rmd_
#' @export rmd_.TukeyHSD
#' @export
rmd_.TukeyHSD <- function(x, xnm, ...) {
  return(c(
    'Tukey Honest Significant Differences (HSD) is provided using <u>**`R`**</u>.',
    '```{r results=\'asis\'}', 
    sprintf(fmt = 'flextable.tzh::as_flextable.TukeyHSD(%s)', xnm),
    '```', 
    '<any-text>'
  ))
}




