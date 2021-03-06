% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{binary_search}
\alias{binary_search}
\title{Binary search}
\usage{
binary_search(vec, x)
}
\arguments{
\item{vec}{\verb{[numeric]} A sorted vector of numbers.}

\item{x}{\verb{[numeric(1)]} The search value.}
}
\value{
The index of \code{x} in \code{vec}. If \code{x} isn't in \code{vec}, returns 0.
}
\description{
Use the binary search algorithm to efficiently find the index of elements in a vector.
}
\examples{
x <- 1:10
binary_search(5)
}
