% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_integer.R
\name{to_integer}
\alias{to_integer}
\alias{to_integer.fixed_design}
\alias{to_integer.gs_design}
\title{Round sample size and events}
\usage{
to_integer(x, ...)

\method{to_integer}{fixed_design}(x, round_up_final = TRUE, ratio = x$input$ratio, ...)

\method{to_integer}{gs_design}(x, round_up_final = TRUE, ratio = x$input$ratio, ...)
}
\arguments{
\item{x}{An object returned by fixed_design_xxx() and gs_design_xxx().}

\item{...}{Additional parameters (not used).}

\item{round_up_final}{Events at final analysis is rounded up if \code{TRUE};
otherwise, just rounded, unless it is very close to an integer.}

\item{ratio}{Positive integer for randomization ratio (experimental:control).
A positive integer will result in rounded sample size, which is a multiple of (ratio + 1).
A positive non-integer will result in round sample size, which may not be a multiple of (ratio + 1).
A negative number will result in an error.}
}
\value{
A list similar to the output of fixed_design_xxx() and gs_design_xxx(),
except the sample size is an integer.
}
\description{
Round sample size and events
}
\details{
For the sample size of the fixed design:
\itemize{
\item When \code{ratio} is a positive integer, the sample size is rounded up to a multiple of \code{ratio + 1}
if \code{round_up_final = TRUE}, and just rounded to a multiple of \code{ratio + 1} if \code{round_up_final = FALSE}.
\item When \code{ratio} is a positive non-integer, the sample size is rounded up if \code{round_up_final = TRUE},
(may not be a multiple of \code{ratio + 1}), and just rounded if \code{round_up_final = FALSE} (may not be a multiple of \code{ratio + 1}).
Note the default \code{ratio} is taken from \code{x$input$ratio}.
}

For the number of events of the fixed design:
\itemize{
\item If the continuous event is very close to an integer within 0.01 differences, say 100.001 or 99.999, then the integer events is 100.
\item Otherwise, round up if \code{round_up_final = TRUE} and round if \code{round_up_final = FALSE}.
}

For the sample size of group sequential designs:
\itemize{
\item When \code{ratio} is a positive integer, the final sample size is rounded to a multiple of \code{ratio + 1}.
\itemize{
\item For 1:1 randomization (experimental:control), set \code{ratio = 1} to round to an even sample size.
\item For 2:1 randomization, set \code{ratio = 2} to round to a multiple of 3.
\item For 3:2 randomization, set \code{ratio = 4} to round to a multiple of 5.
\item Note that for the final analysis, the sample size is rounded up to the nearest multiple of \code{ratio + 1} if \code{round_up_final = TRUE}.
If \code{round_up_final = FALSE}, the final sample size is rounded to the nearest multiple of \code{ratio + 1}.
}
\item When \code{ratio} is positive non-integer, the final sample size MAY NOT be rounded to a multiple of \code{ratio + 1}.
\itemize{
\item The final sample size is rounded up if \code{round_up_final = TRUE}.
\item Otherwise, it is just rounded.
}
}

For the events of group sequential designs:
\itemize{
\item For events at interim analysis, it is rounded.
\item For events at final analysis:
\itemize{
\item If the continuous event is very close to an integer within 0.01 differences, say 100.001 or 99.999, then the integer events is 100.
\item Otherwise, final events is rounded up if \code{round_up_final = TRUE} and rounded if \code{round_up_final = FALSE}.
}
}
}
\examples{
library(dplyr)
library(gsDesign2)

# Average hazard ratio
\donttest{
x <- fixed_design_ahr(
  alpha = .025, power = .9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 1),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12, hr = c(1, .6),
    dropout_rate = .001
  ),
  study_duration = 36
)
x \%>\%
  to_integer() \%>\%
  summary()

# FH
x <- fixed_design_fh(
  alpha = 0.025, power = 0.9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    hr = c(1, .6),
    dropout_rate = .001
  ),
  rho = 0.5, gamma = 0.5,
  study_duration = 36, ratio = 1
)
x \%>\%
  to_integer() \%>\%
  summary()

# MB
x <- fixed_design_mb(
  alpha = 0.025, power = 0.9,
  enroll_rate = define_enroll_rate(duration = 18, rate = 20),
  fail_rate = define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12, hr = c(1, .6),
    dropout_rate = .001
  ),
  tau = Inf, w_max = 2,
  study_duration = 36, ratio = 1
)
x \%>\%
  to_integer() \%>\%
  summary()
}
\donttest{
# Example 1: Information fraction based spending
gs_design_ahr(
  analysis_time = c(18, 30),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
  lower = gs_b,
  lpar = c(-Inf, -Inf)
) \%>\%
  to_integer() \%>\%
  summary()

gs_design_wlr(
  analysis_time = c(18, 30),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
  lower = gs_b,
  lpar = c(-Inf, -Inf)
) \%>\%
  to_integer() \%>\%
  summary()

gs_design_rd(
  p_c = tibble::tibble(stratum = c("A", "B"), rate = c(.2, .3)),
  p_e = tibble::tibble(stratum = c("A", "B"), rate = c(.15, .27)),
  weight = "ss",
  stratum_prev = tibble::tibble(stratum = c("A", "B"), prevalence = c(.4, .6)),
  info_frac = c(0.7, 1),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
  lower = gs_b,
  lpar = c(-Inf, -Inf)
) \%>\%
  to_integer() \%>\%
  summary()

# Example 2: Calendar based spending
x <- gs_design_ahr(
  upper = gs_spending_bound,
  analysis_time = c(18, 30),
  upar = list(
    sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL,
    timing = c(18, 30) / 30
  ),
  lower = gs_b,
  lpar = c(-Inf, -Inf)
) \%>\% to_integer()

# The IA nominal p-value is the same as the IA alpha spending
x$bound$`nominal p`[1]
gsDesign::sfLDOF(alpha = 0.025, t = 18 / 30)$spend
}
}
