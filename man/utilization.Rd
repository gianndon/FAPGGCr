% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilization.R
\name{utilization}
\alias{utilization}
\title{Utilization Function}
\usage{
utilization(c, mu, lambda)
}
\arguments{
\item{c}{number of servers}

\item{mu}{service/processing rate}

\item{lambda}{arrival/incoming rate}
}
\value{
numeric vector, Returns the value of the utilization of the system.
}
\description{
Utilization (work load) of the system
}
\examples{
utilization(c=2, mu=3, lambda=4)
utilization(2, 3, 4)
}
