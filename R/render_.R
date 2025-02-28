

#' @title Create and Render R Markdown files from an R project
#' 
#' @description ..
#' 
#' @param content an R object
#' 
#' @param document \link[base]{character} scalar, type of output document,
#' `'html'` (default), `'word'` or `'pdf'`
#' 
#' @param path \link[base]{character} scalar 
#' 
#' @param filename \link[base]{character} scalar
#' 
#' @param author \link[base]{character} scalar, author's name and/or email
#' 
#' @param autofold \link[base]{logical} scalar
#' 
#' @param rmd.rm \link[base]{logical} scalar, whether to remove the generated `'.rmd'` file,
#' default `TRUE`
#' 
#' @param ... ..
#' 
#' @examples
#' library(flextable)
#' library(ggplot2)
#' list(
#'   '`numeric`' = 1:3,
#'   '`flextable`' = flextable(head(swiss)),
#'   '`ggplot`' = ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'   '`htest` & `power.htest`' = list(
#'    t.test(mpg ~ am, data = mtcars),
#'    power.t.test(power = .90, delta = 1)
#'   )
#' ) |> render_(filename = 'test')
#' @importFrom cli col_cyan
#' @importFrom rmarkdown render
#' @importFrom utils citation
#' @export
render_ <- function(
    content, 
    path = tempdir(),
    document = c('html', 'word', 'pdf'),
    filename = stop('must specify `filename` explicitly'),
    autofold = (document == 'html'),
    author = 'tingting.zhan@jefferson.edu',
    rmd.rm = TRUE,
    ...
) {
  
  document <- match.arg(document)
  
  path <- file.path(path, document)
  dir.create(path = path, showWarnings = FALSE, recursive = TRUE)
  
  if (length(filename) != 1L || !is.character(filename) || is.na(filename) ||
      grepl(pattern = '\\:', x = filename)) stop('`filename` must be len-1 character, without ', sQuote(':'))
  frmd <- file.path(path, sprintf(fmt = '%s %s.rmd', format.Date(Sys.Date()), filename))
  fout <- file.path(path, sprintf(fmt = '%s %s.%s', format.Date(Sys.Date()), filename, switch(document, word = 'docx', document)))
  if (file.exists(fout)) {
    if (document == 'word') system('osascript -e \'quit app "Word"\'') # Word will not automatically close when the .docx file is deleted
    file.remove(fout)
    cat(col_cyan(c('Existing', sQuote(basename(fout)), 'removed\n')))
  }
  
  lrmd <- c( # .rmd lines
    '---',
    sprintf(fmt = 'title: %s', filename),
    sprintf(fmt = 'author: %s', author),
    sprintf(fmt = 'date: %s', format.POSIXct(Sys.time())),
    sprintf(fmt = switch(document, html =, word = {
      'output: %s_document'
    }, pdf = {
      'output: %s_document:\nlatex_engine: xelatex' # not sure if this is wrong
      # 'output: %s_document: latex_engine: xelatex'
    }), document), 
    # Unicode support; https://bookdown.org/yihui/rmarkdown/pdf-document.html
    if (document != 'html') 'always_allow_html: true', # else NULL
    '---',
    
    # https://stackoverflow.com/questions/34906002/increase-width-of-entire-html-rmarkdown-output
    '<style type=\"text/css\">',
    '  .main-container {',
    '    max-width: 1800px;',
    '    margin-left: auto;',
    '    margin-right: auto;',
    '  }',
    '</style>',
    '```{r setup, include = FALSE}',
    'knitr::opts_chunk$set(echo = FALSE)',
    'options(bitmapType = \'cairo\')', # for correct unicode support; DO I STILL NEED THIS ??
    # 'library(tippy)', # Tingting has decided to remove the use of ?tippy::tippy_this
    # 'library(flextable)', # try to write this as `::`
    # 'library(htmlwidgets)', # try to write this as `::`
    '```'
  )
  
  
  content <- content[lengths(content) > 0L]
  nms <- names(content)
  if (!length(nms) || anyNA(nms) || !all(nzchar(nms))) stop('names must be complete')
  
  if (!(nsection <- length(content))) return(invisible())
  for (i in seq_len(nsection)) {
    lrmd <- c(
      lrmd, 
      '\n', # must use an extra '\n' (to separate from previous 'character')
      sprintf(fmt = '## %s', nms[i]), # I like title font smaller than '#' or '##'
      if (autofold) '<details>',
      '\n\n', 
      rmd_(x = content[[i]], xnm = sprintf(fmt = 'content[[%d]]', i)), 
      '\n\n',
      '</details>' # if (autofold) 
    )
  }
  
  lrmd <- c(
    lrmd, 
    '\n',
    '## Citations',
    '<details>',
    unlist(lapply(extract_pkg_name(lrmd), FUN = function(i) rmd_.bibentry(citation(package = i)))),
    '</details>'
  )
  
  writeLines(text = lrmd, con = frmd, sep = '\n')
  
  # knitr::knit2html(input = frmd, output = fout)
  # error: should call ?rmarkdown::render instead of ?knitr::knit2html because Rmd appears to be an R Markdown v2 document. 
  render(input = frmd, output_file = fout, intermediates_dir = path, quiet = TRUE)
  system(paste0('open \'', normalizePath(fout), '\''))
  
  if (rmd.rm) file.remove(frmd) else system(paste0('open \'', normalizePath(frmd), '\''))
  
  return(invisible(fout))
}








