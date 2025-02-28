#' @title Create R Markdown Script for \link[DanielBiostatistics10th]{binTab} Object
#' 
#' @description
#' Method dispatch to \link[DanielBiostatistics10th]{binTab} for S3 generic `rmd_` (in a different master package).
#' 
#' @param x a \link[DanielBiostatistics10th]{binTab}
#' 
#' @param xnm \link[base]{language} or \link[base]{character} scalar, call of `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [rmd_.binTab] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' library(DanielBiostatistics10th)
#' list(
#'   '`binTab`' = binTab(matrix(c(7L, 3L, 8L, 6L), nrow = 2L))
#' ) |> render_(filename = 'binTab')
#' @export rmd_.binTab
#' @export
rmd_.binTab <- function(x, xnm = substitute(x), ...) {
  if (is.language(xnm)) xnm <- deparse1(xnm)
  if (!is.character(xnm) || length(xnm) != 1L || is.na(xnm) || !nzchar(xnm)) stop('illegal `xnm`')
  
  dnm <- dimnames(x)
  
  # '\u03ba' # Unicode kappa
  
  c(
    sprintf(fmt = 'Sensitivity, specificity and predictive values, as well as their 95%% exact confidence intervals, are provided for the 2-by-2 table of `%s` and `%s`. Cohen\'s $\\kappa$ coefficient for agreement is provided by <u>**`R`**</u> package <u>**`vcd`**</u>.',
            names(dnm)[1L], names(dnm)[2L]),
    '```{r results = \'asis\'}', 
    sprintf(fmt = 'as_flextable(%s)', xnm),
    '```', 
    '```{r comment = NA}', 
    paste0('cat(Sprintf.binTab(', xnm, '), sep = \'\n\')'), # how to put in `prevalence` here??
    '```'
  )
}

