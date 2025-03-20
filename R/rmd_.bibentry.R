

#' @rdname rmd_
#' 
#' @examples 
#' list(
#'  stringi = 'String manipulation by <u>**`R`**</u> package <u>**`stringi`**</u>.',
#'  texreg = 'R regression output to LaTeX or HTML by <u>**`R`**</u> package <u>**`texreg`**</u>.',
#'  stats = 'Linear regression by <u>**`R`**</u> package <u>**`stats`**</u>.'
#' ) |> render_(file = 'bibentry')
#' 
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed stri_replace_all_regex
#' @export rmd_.bibentry
#' @export
rmd_.bibentry <- function(x, ...) {
  
  pkg <- attr(x, which = 'package', exact = TRUE)
  
  return(sprintf(
    fmt = '<u>**`%s`**</u> %s\n', 
    if (pkg == 'base') 'R' else pkg, 
    x |> bibentry2text()
  ))
  
}



#' @title Slight Improvement over `utils:::format.bibentry`
#' 
#' @param x a \link[utils]{citation} and/or \link[utils]{bibentry}
#' 
#' @details
#' Function [bibentry2text()] beautifies the output from 
#' function `utils:::format.bibentry(., style = 'text')`
#' in the following ways.
#' \itemize{
#' \item{Line break `'\n'` is replaced by a white space;}
#' \item{Fancy quotes \eqn{``}, \eqn{''}, \eqn{`} and \eqn{'} are removed;}
#' \item{doi entries are shown as URLs with labels in R markdown grammar.}
#' }
#' 
#' @examples
#' 'stringi' |> citation() |> bibentry2text()
#' 'texreg' |> citation() |> bibentry2text()
#' @keywords internal
#' @export
bibentry2text <- function(x) {
  
  format_doi <- function(x, regex_pattern, fixed_pattern, fixed_replacement) {
    
    has_doi <- x |> grepl(pattern = regex_pattern)
    
    doi <- x[has_doi] |> 
      stri_extract_all_regex(pattern = regex_pattern)
    if (!all(lengths(doi) == 1L)) stop('one citation cannot have >1 doi')
    
    x[has_doi] <- doi |>
      unlist() |>
      stri_replace_all_fixed(pattern = fixed_pattern, replacement = fixed_replacement, vectorize_all = FALSE) |> # to get [topic](url)
      stri_replace_all_regex(str = x[has_doi], pattern = regex_pattern)
    
    return(x)
    
  }
  
  x |> 
    url2doi() |>
    format(style = 'text') |> # ?utils:::format.citation -> ?utils:::format.bibentry
    gsub(pattern = '\n', replacement = ' ') |>
    gsub(pattern = '\"|\u201c|\u201d|\u2018|\u2019', replacement = '') |>
    # '\u201c|\u201d' # quotation marks created by ?base::dQuote
    # '\u2018|\u2019' # quotation marks created by ?base::sQuote
    format_doi(
      regex_pattern = '( doi:)(.*?)( <https://doi.org/)(.*?)(>)(.|,)', 
      fixed_pattern = c(' doi:', ' <https://doi.org/', '>'), 
      fixed_replacement = c(' [doi:', '](https://doi.org/', ')')
    )
  
}




if (FALSE) { # disabled for ?devtools::check
  ct = installed.packages() |>
    rownames() |>
    lapply(FUN = citation) # slow
  ct |> lapply(FUN = rmd_.bibentry)
} 


#' @title Correct `doi` in `url`
#' 
#' @param x a \link[utils]{citation} and/or \link[utils]{bibentry}
#' 
#' @returns
#' Function [url2doi()] returns a \link[utils]{citation} and/or \link[utils]{bibentry}.
#' 
#' @examples
#' 'stringi' |> citation() # using doi field, correct
#' 'texreg' |> citation() # doi in url field, not good!
#' 
#' # \pkg{texreg} has textVersion hard-coded in CITATION file
#' # therefore 'textVersion' cannot be updated
#' 'texreg' |> citation() |> url2doi() # only a partial fix 
#' 
#' 'robslopes' |> citation() |> toBibtex()
#' 'robslopes' |> citation() |> url2doi() |> toBibtex() # complete fix!
#' @keywords internal
#' @export
url2doi <- function(x) {
  
  ret <- unclass(x) |> 
    lapply(FUN = function(b) { # (b = unclass(x)[[1L]])
      url_ <- b[['url']] # name clash ?base::url
      if (!length(url_)) return(b)
      if (!grepl(pattern = 'https://doi.org/', x = url_)) return(b)
      doi <- gsub(pattern = 'https://doi.org/', replacement = '', x = url_)
      if (length(b[['doi']])) {
        if (!identical(b[['doi']], doi)) stop()
      } else b[['doi']] <- doi
      b[['url']] <- NULL
      return(b)
    })
  
  attributes(ret) <- attributes(x)
  return(ret)
  
}


