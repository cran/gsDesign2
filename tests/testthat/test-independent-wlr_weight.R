test_that("test wlr_weight_1", {
  expect_equal(gsDesign2::wlr_weight_1(), 1)
})

test_that("test wlr_weight_n", {
  enroll_rate <- define_enroll_rate(
    duration = c(2, 2, 30),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  total_time <- 36
  analysis_time <- 12

  arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1, total_time = total_time)
  arm0 <- arm$arm0
  arm1 <- arm$arm1

  prob0 <- gsDesign2:::prob_risk(arm0, analysis_time, total_time)
  prob1 <- gsDesign2:::prob_risk(arm1, analysis_time, total_time)

  expect_equal(
    gsDesign2::wlr_weight_n(x = analysis_time, arm0 = arm0, arm1 = arm1, power = 2),
    (2 * (0.5 * prob0 + 0.5 * prob1))^2
  )
})
