

#' @title Extract Package Name(s)
#' 
#' @description
#' Extract package name(s) from a \link[base]{character} \link[base]{vector}
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @param pattern \link[base]{regex}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @examples
#' extract_pkg_name('<u>**`R`**</u> package <u>**`patchwork`**</u>')
#' extract_pkg_name(c('[R] package [patchwork]', '[ggplot2]'), pattern = '(?<=\\[)(.*?)(?=\\])')
#' # exception handling
#' extract_pkg_name(character())
#' extract_pkg_name('')
#' extract_pkg_name(NA_character_)
#' @importFrom stringi stri_extract_all_regex
#' @export
extract_pkg_name <- function(
    x, 
    pattern = '(?<=\\<u\\>\\*\\*`)(.*?)(?=`\\*\\*\\<\\/u\\>)',
    ...
) {
  
  x <- x[!is.na(x)]
  
  # workhorse
  ret <- x |>
    stri_extract_all_regex(pattern = pattern) |>
    unlist(use.names = FALSE) |>
    unique.default()
  
  if (any(!nzchar(ret))) stop('should not happen')
  # if (anyNA(ret)) may happen! (`x = ''`)
  
  return(setdiff(c('base', ret[!is.na(ret)]), y = 'R'))
  
}
