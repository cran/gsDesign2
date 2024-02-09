## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "png",
  dpi = 96,
  fig.retina = 1,
  fig.width = 7.2916667,
  fig.asp = 0.618,
  fig.align = "center",
  out.width = "80%"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(gsDesign)
library(gsDesign2)
library(knitr)
library(dplyr)
library(gt)
library(ggplot2)

## -----------------------------------------------------------------------------
enroll_rate <- define_enroll_rate(
  duration = c(2, 2, 2, 6),
  rate = (1:4) / 4
)

enroll_rate %>% gt()

## -----------------------------------------------------------------------------
median_surv <- 12

fail_rate <- define_fail_rate(
  duration = c(4, Inf),
  fail_rate = log(2) / median_surv,
  hr = c(1, .6),
  dropout_rate = .001
)

fail_rate %>% gt()

## -----------------------------------------------------------------------------
alpha <- .025
beta <- .1 # 1 - targeted power
d <- fixed_design_ahr(
  enroll_rate = enroll_rate, # Relative enrollment rates
  fail_rate = fail_rate, # Failure rates from above
  alpha = alpha, # Type I error
  power = 1 - beta, # Type II error = 1 - power
  study_duration = 36 # Planned trial duration
)

## -----------------------------------------------------------------------------
d %>%
  summary() %>%
  as_gt()

## -----------------------------------------------------------------------------
d$enroll_rate %>% gt()

## ----echo=FALSE---------------------------------------------------------------
ggplot(
  data = tibble(t = (0:50) / 50, `f(t)` = 2 - 2 * pnorm(qnorm(1 - .0125) / sqrt(t))),
  aes(x = t, y = `f(t)`)
) +
  geom_line()

## -----------------------------------------------------------------------------
design1s <- gs_design_ahr(
  alpha = alpha,
  beta = beta,
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  analysis_time = c(16, 26, 36), # Calendar time of planned analyses
  upper = gs_spending_bound, # Spending function bound for efficacy
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025), # Specify spending function and total Type I error
  lower = gs_b, lpar = rep(-Inf, 3), # No futility bound
  info_scale = "h0_h1_info"
)

## -----------------------------------------------------------------------------
design1s %>%
  summary() %>%
  as_gt(
    title = "1-sided group sequential bound using AHR method",
    subtitle = "Lan-DeMets spending to approximate O'Brien-Fleming bound"
  )

## ----class.source = 'fold-show'-----------------------------------------------
x <- gsDesign(k = 3, test.type = 1, timing = design1s$analysis$info_frac, sfu = sfLDOF)
cat(
  "gsDesign\n  Upper bound: ", x$upper$bound,
  "\n  Cumulative boundary crossing probability (H0): ", cumsum(x$upper$prob[, 1]),
  "\n  Timing (IF): ", x$timing,
  "\ngs_design_ahr\n  Upper bound: ", design1s$bound$z,
  "\n  Cumulative boundary crossing probability (H0): ", design1s$bound$probability0,
  "\n  Timinng (IF): ", design1s$analysis$info_frac,
  "\n"
)

## -----------------------------------------------------------------------------
design2ss <- gs_design_ahr(
  alpha = alpha,
  beta = beta,
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  analysis_time = c(16, 26, 36), #  Calendar analysis times
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  lower = gs_spending_bound,
  lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.025),
  h1_spending = FALSE # This specifies futility testing with spending under NULL
)

## ----message=FALSE------------------------------------------------------------
design2ss %>%
  summary() %>%
  as_gt(
    title = "2-sided symmetric group sequential bound using AHR method",
    subtitle = "Lan-DeMets spending to approximate O'Brien-Fleming bound"
  )

## -----------------------------------------------------------------------------
ggplot(
  data = design2ss$analysis %>% left_join(design2ss$bound, by = "analysis"),
  aes(x = event, y = z, group = bound)
) +
  geom_line(aes(linetype = bound)) +
  geom_point() +
  ggtitle("2-sided symmetric bounds with O'Brien-Fleming-like spending")

## -----------------------------------------------------------------------------
design2sa <- gs_design_ahr(
  alpha = alpha,
  beta = beta,
  enroll_rate = enroll_rate,
  fail_rate = fail_rate,
  analysis_time = c(12, 16, 26, 36),
  upper = gs_spending_bound,
  upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025), # Same efficacy bound as before
  test_lower = c(FALSE, TRUE, TRUE, TRUE), # Only test efficacy after IA1
  lower = gs_b,
  lpar = c(rep(qnorm(.05), 2), -Inf, -Inf) # Fixed lower bound at first 2 analyses
)

## -----------------------------------------------------------------------------
design2sa %>%
  summary() %>%
  as_gt(
    title = "2-sided asymmetric group sequential bound using AHR method",
    subtitle = "Lan-DeMets spending to approximate O'Brien-Fleming bound
    for efficacy, futility disaster check at IA1, IA2 only"
  )

