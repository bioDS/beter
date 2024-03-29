% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_utils.r
\name{format_list}
\alias{format_list}
\title{Convert numeric elements in list to string}
\usage{
format_list(x, scientific = FALSE, trim = TRUE)
}
\arguments{
\item{x}{a list}

\item{scientific}{whether to keep or not the scientific notation}

\item{trim}{suppress justifying values to right}
}
\value{
formatted list
}
\description{
Recursively converts numeric elements in list to string using the `format` function.
This is done to guarantee that scientific notation is not used.
}
\keyword{internal}
