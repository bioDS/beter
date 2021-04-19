% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequences.r
\name{sequences2xml}
\alias{sequences2xml}
\title{Convert sequences to BEAST xml data block.}
\usage{
sequences2xml(sequences, data = list())
}
\arguments{
\item{sequences}{parsed from file}

\item{data}{other variables, such as specification of dataType}
}
\value{
BEAST xml data block
}
\description{
This functions converts the parsed sequences into an XML data block for BEAST.
}