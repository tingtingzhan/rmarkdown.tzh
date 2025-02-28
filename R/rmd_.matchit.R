
#' @title R Markdown Format of \link[MatchIt]{matchit} Object
#' 
#' @description
#' ..
#' 
#' 
#' @param x a \link[MatchIt]{matchit}
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' library(MatchIt)
#' m = matchit(treat ~ age+educ+race+nodegree+married+re74+re75, data = lalonde)
#' list(
#'  '`matchit1`' = m,
#'  '`matchit2`' = m |> summary(addlvariables = 're78')
#' ) |> render_(filename = 'matchit')
#' @importFrom stats formula
#' @name rmd_matchit
#' @export rmd_.summary.matchit
#' @export
rmd_.summary.matchit <- function(x, xnm, ...) {
  n_all <- x$nn['All', ] # stop if error
  n_matched <- x$nn['Matched', ] # stop if error
  fom <- formula(x)
  txt <- sprintf(fmt = 'Nonparametric matching of `%s` based on %s was performed using <u>**`R`**</u> package <u>**`MatchIt`**</u> from %d `control` and %d `treated` subjects. The matched data contains %d `control` and %d `treated` subjects.',
                 as.character(fom[[2L]]), # `arm`
                 paste0('`', all.vars(fom[[3L]]), '`', collapse = ', '), # matching criterion
                 n_all[1L], n_all[2L], n_matched[1L], n_matched[2L])
  
  c(
    txt,
    '',
    '```{r results = \'asis\'}', 
    sprintf(fmt = 'as_flextable.summary.matchit(%s)', xnm),
    '```', 
    '<any-text>'
  )
}




#' @rdname rmd_matchit
#' @export rmd_.matchit
#' @export
rmd_.matchit <- function(x, xnm, ...) {
  
  # I do *not* know how to deal with `...` here!!!!
  
  #xsum <- summary(x, ...) # ?MatchIt:::summary.matchit
  
  rmd_.summary.matchit(
    x = summary(x), # ?MatchIt:::summary.matchit
    #xnm = sprintf(fmt = 'summary(%s, ...)', xnm), # actually I am not sure if this is correct!!!
    xnm = sprintf(fmt = 'summary(%s)', xnm), # this is correct for sure
    ...
  )
  
}





