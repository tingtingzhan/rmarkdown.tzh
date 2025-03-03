% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmd_.venn.R
\name{rmd_.venn}
\alias{rmd_.venn}
\title{Create R Markdown Script for \link[venn.tzh]{venn}}
\usage{
\method{rmd_}{venn}(x, xnm, ...)
}
\arguments{
\item{x}{a \link[venn.tzh]{venn} object}

\item{xnm}{\link[base]{character} scalar, \link[base]{deparse}d call of \code{x}}

\item{...}{additional parameters, currently not in use}
}
\description{
Method dispatch to \link[venn.tzh]{venn} for S3 generic \code{rmd_} (in a different master package).
}
\examples{
library(venn.tzh)
list(
  '`venn`' = venn(list(A = state.name[1:20], B = state.name[15:30]))
) |> render_(filename = 'venn')
}
