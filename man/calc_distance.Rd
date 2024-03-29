% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_functions.R
\name{calc_distance}
\alias{calc_distance}
\title{Calculate different distance metrics}
\usage{
calc_distance(baseline, comparison, dist.metric = c("mahalanobis", "sed"))
}
\arguments{
\item{baseline}{a matrix containing variables for the baseline period}

\item{comparison}{a matrix containing variables for the comparison period}

\item{dist.metric}{a character vector indicating if Mahalanobis ("mahalanobis") or SED ("sed") should be calculated}
}
\value{
a matrix of distance measures for the comparison period
}
\description{
Calculates either the Mahalanobis distance or Standardized
Euclidean Distance between baseline (historical) and comparison (future) climates
}
