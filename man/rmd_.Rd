% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmd_.R, R/rmd_.gDesc.R, R/rmd_plot.R,
%   R/rmd_print.R
\name{rmd_}
\alias{rmd_}
\alias{rmd_.data.frame}
\alias{rmd_.array}
\alias{rmd_.list}
\alias{rmd_.numeric}
\alias{rmd_.character}
\alias{rmd_.noquote}
\alias{rmd_.gDesc}
\alias{rmd_.gList}
\alias{rmd_.trellis}
\alias{rmd_.consort}
\alias{rmd_.gg}
\alias{rmd_.htest}
\alias{rmd_.power.htest}
\alias{rmd_.flextable}
\alias{rmd_.htmlwidget}
\title{R Markdown Script}
\usage{
rmd_(x, ...)

\method{rmd_}{data.frame}(x, xnm, ...)

\method{rmd_}{array}(x, xnm, ...)

\method{rmd_}{list}(x, xnm, ...)

\method{rmd_}{numeric}(x, ...)

\method{rmd_}{character}(x, ...)

\method{rmd_}{noquote}(x, xnm, ...)

\method{rmd_}{gDesc}(x, xnm, ...)

\method{rmd_}{gList}(x, xnm, ...)

\method{rmd_}{trellis}(x, xnm, ...)

\method{rmd_}{consort}(x, xnm, ...)

\method{rmd_}{gg}(x, xnm, ...)

\method{rmd_}{htest}(x, xnm, ...)

\method{rmd_}{power.htest}(x, xnm, ...)

\method{rmd_}{flextable}(x, xnm, ...)

\method{rmd_}{htmlwidget}(x, xnm, ...)
}
\arguments{
\item{x}{an R object}

\item{...}{additional parameters, currently not in use}

\item{xnm}{..}
}
\description{
R Markdown Script
}
\examples{
\donttest{
library(mDFR)
?maxT_santos_test
ds = split(santos1, f = ~ Hr + antigen)
list(
 '`maxT`' = maxT_santos_test(data1 = ds$`18.CEF`, data0 = ds$`0.CEF`)
) |> render_(file = 'maxT')
} # slow

set.seed(100)
x = matrix(rnorm(50), ncol = 5, dimnames = list(NULL, LETTERS[1:5]))
y = matrix(rnorm(30), ncol = 3, dimnames = list(NULL, letters[1:3]))
library(htest.tzh); list(
  '`htest_array`' = outer.cor.test(x, y)
) |> render_(file = 'htest_array')

library(rpart.tzh); library(rpart); list(
  '`rpart`' = rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, model = TRUE)
) |> render_(file = 'rpart')

library(venn.tzh); list(
  '`venn`' = venn(list(A = state.name[1:20], B = state.name[15:30]))
) |> render_(file = 'venn')
list(
  '`data.frame`' = swiss
) |> render_(file = 'data.frame')
list(
  '`matrix`' = VADeaths
) |> render_(file = 'matrix')
library(lattice); Depth = equal.count(quakes$depth, number=8, overlap=.1)
library(consort); data(dispos.data)
list(
 '`trellis`' = xyplot(lat ~ long | Depth, data = quakes),
 '`consort`' = consort_plot(
  data = dispos.data |> subset(subset = !(arm3 \%in\% 'Trt C')),
  orders = list(c(trialno = 'Population'), c(exclusion = 'Excluded'), c(arm = 'Randomized')),
  side_box = c('exclusion'))
) |> render_(file = 'Vanilla Plot')
library(ggplot2); list(
  '`htest`' = t.test(mpg ~ am, data = mtcars),
  '`power.htest`' = power.t.test(power = .90, delta = 1),
  '`ggplot2::ggplot`' = ggplot(mtcars, aes(wt, mpg)) + geom_point(),
  '`GGally::ggmatrix`' = GGally::ggpairs(swiss, columns = c(1:2, 6))
) |> render_(file = 'Explicit Print')
library(flextable); library(reactable); library(plotly); 
list(
 '`flextable::flextable`' = flextable(mtcars),
 '`reactable::reactable`, inherits from `htmlwidget`' = reactable(iris),
 '`htmlwidget`' = list(
   plot_ly(economics, x = ~date, y = ~pop, type = 'scatter', mode = 'markers'),
   plot_ly(z = ~volcano, type = "surface")
 )
) |> render_(file = 'Do Not Say Print')
}
