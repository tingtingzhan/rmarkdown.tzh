


#' @title R Markdown Script of \CRANpkg{magick} Image
#' 
#' @param x a `'magick-image'` object returned from function \link[magick]{image_read}
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' ?magick::image_read
#' library(magick); list(
#'  '`magick-image`' = 'https://jeroen.github.io/images/frink.png' |> image_read()
#' ) |> render_(file = 'magick')
#' @export `rmd_.magick-image`
#' @export
`rmd_.magick-image` <- function(x, xnm, ...) {
  return(c(
    '```{r}',
    sprintf(fmt = '%s |> print(info = FALSE)', xnm), # ?magick:::`print.magick-image`
    '```', 
    '<any-text>'
  ))
  # ?magick::image_write; write to a file
  # ?magick::image_info; height and width
}