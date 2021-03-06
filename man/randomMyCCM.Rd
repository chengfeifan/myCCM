% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/randomMyCCM.R
\name{randomMyCCM}
\alias{randomMyCCM}
\title{Calculate the random CCM value of x and y}
\usage{
randomMyCCM(x, y, lag, E = 2, k = 2, way = 2, ...)
}
\arguments{
\item{x}{A vector represents the time sequence}

\item{y}{A vector represents another time sequence}

\item{lag}{Time lag between x and y}

\item{k}{K nearest neighbors to construct the manifold}

\item{E+1}{The embedded dimension of the manifold}
}
\value{
A list of time lag and correlation number
}
\description{
Takes in two sequences, and resample them, then calculate the value of CCM
}

