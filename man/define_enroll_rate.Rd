% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define.R
\name{define_enroll_rate}
\alias{define_enroll_rate}
\title{Define enrollment rate}
\usage{
define_enroll_rate(duration, rate, stratum = "All")
}
\arguments{
\item{duration}{A numeric vector of ordered piecewise study duration interval.}

\item{rate}{A numeric vector of enrollment rate in each \code{duration}.}

\item{stratum}{A character vector of stratum name.}
}
\value{
An \code{enroll_rate} data frame.
}
\description{
Define the enrollment rate of subjects for a study as following a piecewise
exponential distribution.
}
\details{
The \code{duration} are ordered piecewise for a duration equal to
\eqn{t_i - t_{i-1}}, where \eqn{0 = t_0 < t_i < \cdots < t_M = \infty}.
The enrollment rates are defined in each duration with the same length.

For a study with multiple strata, different duration and rates can be
specified in each stratum.
}
\examples{
# Define enroll rate without stratum
define_enroll_rate(
  duration = c(2, 2, 10),
  rate = c(3, 6, 9)
)

# Define enroll rate with stratum
define_enroll_rate(
  duration = rep(c(2, 2, 2, 18), 3),
  rate = c((1:4) / 3, (1:4) / 2, (1:4) / 6),
  stratum = c(array("High", 4), array("Moderate", 4), array("Low", 4))
)
}
