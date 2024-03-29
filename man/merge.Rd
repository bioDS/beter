% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_lists.r
\name{merge}
\alias{merge}
\alias{add}
\title{Merge two lists}
\usage{
merge(list1, list2)

add(list1, list2)
}
\arguments{
\item{list1}{a list}

\item{list2}{a list}
}
\value{
merged list
}
\description{
Additive and non-additive merging of lists. \code{add} will merge lists additively, this means
that named items shared in both lists are added concatenated. \code{merge} is non-additive
merging and named items from list in the first argument will be replaced by items from
the list in the second argument.
}
\keyword{internal}
