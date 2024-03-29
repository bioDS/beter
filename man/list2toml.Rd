% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list2toml.r
\name{list2toml}
\alias{list2toml}
\title{list to toml}
\usage{
list2toml(x)
}
\arguments{
\item{x}{a named list}
}
\value{
a string vector representing individual lines of TOML representation of \code{x}.
}
\description{
Convert list to a string vector that represents of TOML. Each item of the vector represents
a line of the TOML file.
}
\details{
\code{list2toml} process structured lists recursively by first processing any non-list members
and then processing remaining items of the list type using another call of \code{lit2toml}.
In these calls, the name of the item is used as a \code{parent_name}.
}
\examples{
config = list(
    "xml" = list("chunk" = "<xml>{{tag}}</xml>"),
    "templates" = list(),
    "defaults" = list("tag" = "Just another tag in the TOML!")
     )
list2toml(config)
}
