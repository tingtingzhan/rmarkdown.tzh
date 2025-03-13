

#' @title R Markdown Script of \link[utils]{citation} and/or \link[utils]{bibentry}
#' 
#' @description 
#' R markdown format of a \link[utils]{citation} and/or \link[utils]{bibentry} object.
#' 
#' @param x a \link[utils]{citation} and/or \link[utils]{bibentry}
#'
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [rmd_.bibentry] beautifies the output from 
#' function `utils:::format.bibentry` (with option `style = 'text'`)
#' in the following ways.
#' \itemize{
#' \item{Line break `'\n'` is replaced by a white space;}
#' \item{Fancy quotes \eqn{``}, \eqn{''}, \eqn{`} and \eqn{'} are removed;}
#' \item{doi entries are shown as URLs with labels (in R markdown grammar).}
#' }
#' 
#' @returns 
#' Function [rmd_.bibentry] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @examples 
#' citation() |> rmd_.bibentry()
#' library(ggplot2); 'ggplot2' |> citation() |> rmd_.bibentry()
#' list(
#'  'Methods' = list(
#'   'String manipulation by **`R`** package **`stringi`**.',
#'   'Conversion of R regression output to LaTeX or HTML by **`R`** package **`texreg`**.'
#'  ),
#'  '`bibentry`' = list(
#'   'stringi' |> citation(), # good doi
#'   'texreg' |> citation() # bad doi
#'  )
#' ) |> render_(file = 'bibentry')
#' 
#' # compare
#' 'stringi' |> citation() |> toBibtex() # using doi field, correct
#' 'texreg' |> citation() |> toBibtex() # using url field, not good! have not dealt with this before
#' 
#' \dontrun{ # disabled for ?devtools::check
#' ct = installed.packages() |>
#'  rownames() |>
#'  lapply(FUN = function(i) i |> citation()) # slow
#' 
#' ct |> lapply(FUN = rmd_.bibentry)
#' }
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed stri_replace_all_regex
#' @export rmd_.bibentry
#' @export
rmd_.bibentry <- function(x, ...) {
  
  y <- x |> format(style = 'text') # ?utils:::format.citation -> ?utils:::format.bibentry
  # return may have `length(y) > 1L`
  if (!length(y) || anyNA(y) || any(!nzchar(y))) stop('Package ', sQuote(x), ' bug?')
  
  y <- y |> 
    gsub(pattern = '\n', replacement = ' ') |>
    gsub(pattern = '\"|\u201c|\u201d|\u2018|\u2019', replacement = '')
  # '\u201c|\u201d' # quotation marks created by ?base::dQuote
  # '\u2018|\u2019' # quotation marks created by ?base::sQuote
  
  format_doi <- function(x, regex_pattern, fixed_pattern, fixed_replacement) {
    has_doi <- x |> grepl(pattern = regex_pattern)
    
    doi <- x[has_doi] |> 
      stri_extract_all_regex(pattern = regex_pattern)
    if (!all(lengths(doi) == 1L)) stop('one citation cannot have >1 doi')
    
    x[has_doi] <- doi |>
      unlist() |>
      stri_replace_all_fixed(pattern = fixed_pattern, replacement = fixed_replacement, vectorize_all = FALSE) |> # to get [topic](url)
      stri_replace_all_regex(str = y[has_doi], pattern = regex_pattern)
    
    return(x)
  }

  y <- y |> format_doi( # doi in doi field
    regex_pattern = '( doi:)(.*?)( <https://doi.org/)(.*?)(>)(.|,)', 
    fixed_pattern = c(' doi:', ' <https://doi.org/', '>'), 
    fixed_replacement = c(' [doi:', '](https://doi.org/', ')')
  )

  if (FALSE) {
    # no need! rmarkdown rendered product is the same :)))
    y <- y |> format_doi( # doi in url field (e.g., \pkg{texreg})
      regex_pattern = '( <https://doi.org/)(.*?)(>)(.|,)', 
      fixed_pattern = c(' <https://doi.org/', '>'), 
      fixed_replacement = c(' https://doi.org/', '')
    ) # sufficiently okay!
  }
  
  pkg <- attr(x, which = 'package', exact = TRUE)
  
  return(sprintf(
    fmt = '<u>**`%s`**</u> %s\n', 
    if (pkg == 'base') 'R' else pkg, 
    y))
  
}


# @title Wrongly put `doi` in `url`
# 
# @param x a `'Bibtex'` object (return of function \link[utils]{toBibtex})
# 
# @examples
# citation('VGAM') |> toBibtex() # really weird structure..
# 
# @keywords internal
# @export
#doi_in_url <- function(x) {
#  if (!('url' %in% names(x))) return(FALSE)
#  grepl(pattern = 'https://doi.org/', x = x[['url']])
#}


