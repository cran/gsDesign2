% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pwe.R
\name{s2pwe}
\alias{s2pwe}
\title{Approximate survival distribution with piecewise exponential distribution}
\usage{
s2pwe(times, survival)
}
\arguments{
\item{times}{Positive increasing times at which survival distribution is provided.}

\item{survival}{Survival (1 - cumulative distribution function) at specified \code{times}.}
}
\value{
A tibble containing the duration and rate.
}
\description{
Converts a discrete set of points from an arbitrary survival distribution
to a piecewise exponential approximation.
}
\section{Specification}{

\if{latex}{
 \itemize{
   \item Validate if input times is increasing positive finite numbers.
   \item Validate if input survival is numeric and same length as input times.
   \item Validate if input survival is positive, non-increasing, less than or equal to 1 and greater than 0.
   \item Create a tibble of inputs times and survival.
   \item Calculate the duration, hazard and the rate.
   \item Return the duration and rate by \code{s2pwe}
 }
 }
\if{html}{The contents of this section are shown in PDF user manual only.}
}

\examples{
# Example: arbitrary numbers
s2pwe(1:9, (9:1) / 10)
# Example: lognormal
s2pwe(c(1:6, 9), plnorm(c(1:6, 9), meanlog = 0, sdlog = 2, lower.tail = FALSE))
}
