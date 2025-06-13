# Test get_combo_weight ----
test_that("get_combo_weight output correct rho1, rho2, gamma1, gamm2", {

  weight <- gsDesign2:::get_combo_weight(rho = 1, gamma = 0, tau = -1)
  weight <- eval(parse(text = weight[[1]]))

  expect_equal(weight$param$rho, 1)
  expect_equal(weight$param$gamma, 0)
  expect_equal(weight$param$tau, NULL)
})


# Test get_combo_weight tau not equal to -1 ----
test_that("get_combo_weight output correct tau1, tau3", {

  weight <- gsDesign2:::get_combo_weight(rho = 1, gamma = 0, tau = 1)
  weight <- eval(parse(text = weight[[1]]))

  expect_equal(weight$param$tau, 1)
})

# Test gs_delta_combo ----
test_that("gs_delta_combo correctly use gs_delta_wlr 1", {

  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)

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

  arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1, total_time = 1e6)

  delta <- gsDesign2:::gs_delta_combo(
    arm0 = arm$arm0, arm1 = arm$arm1,
    tmax = 30, rho = rho, gamma = gamma, tau = rep(-1, length(rho)),
    approx = "asymptotic", normalization = FALSE
  )

  res <- list(
    "rho" = rho,
    "gamma" = gamma,
    "tau" = tau,
    "arm" = arm,
    "delta" = delta
  )

  rho <- res$rho
  gamma <- res$gamma
  tau <- res$tau
  arm <- res$arm
  delta <- res$delta

  for (i in 1:4) {
    weight_test1 <- gsDesign2:::get_combo_weight(rho[i], gamma[i], tau[i])
    delta_test1 <- gsDesign2:::gs_delta_wlr(
      arm0 = arm$arm0, arm1 = arm$arm1,
      tmax = 30, weight = eval(parse(text = weight_test1)),
      approx = "asymptotic", normalization = FALSE
    )

    expect_identical(delta[i], delta_test1)
  }
})

# Test gs_sigma2_combo ----
test_that("gs_sigma2_combo correctly use gs_sigma2_wlr 1", {

  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)

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
  arm <- gs_create_arm(enroll_rate, fail_rate, ratio = 1, total_time = 1e6)

  sigma2 <- gsDesign2:::gs_sigma2_combo(
    arm0 = arm$arm0, arm1 = arm$arm1, tmax = 30,
    rho = rho, gamma = gamma, tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )
  rho1 <- outer(rho, rho, function(x, y) (x + y) / 2)
  gamma1 <- outer(gamma, gamma, function(x, y) (x + y) / 2)

  res <- list(
    "rho1" = rho1,
    "gamma1" = gamma1,
    "tau" = tau,
    "arm" = arm,
    "sigma2" = sigma2
  )

  rho1 <- res$rho1
  gamma1 <- res$gamma1
  tau <- res$tau
  arm <- res$arm
  sigma2 <- res$sigma2

  for (i in 1:4) {
    for (j in 1:4) {
      weight_test_ij <- gsDesign2:::get_combo_weight(rho1[i, j], gamma1[i, j], tau[i])
      sigma_ij <- gsDesign2:::gs_sigma2_wlr(
        arm0 = arm$arm0, arm1 = arm$arm1,
        tmax = 30, weight = eval(parse(text = weight_test_ij)),
        approx = "asymptotic"
      )

      expect_equal(sigma2[i, j], sigma_ij)
    }
  }
})

# Test gs_info_combo ----
test_that("gs_info_combo correctly use gs_info_wlr 1", {
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)

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

  info_combo <- gsDesign2:::gs_info_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1, # Experimental:Control randomization ratio
    event = NULL, # Events at analyses
    analysis_time = 30, # Times of analyses
    rho = rho,
    gamma = gamma,
    tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )

  res <- list(
    "rho" = rho,
    "gamma" = gamma,
    "tau" = tau,
    "enroll_rate" = enroll_rate,
    "fail_rate" = fail_rate,
    "info_combo" = info_combo
  )

  rho <- res$rho
  gamma <- res$gamma
  tau <- res$tau
  enroll_rate <- res$enroll_rate
  fail_rate <- res$fail_rate
  info_combo <- res$info_combo

  for (i in 1:4) {
    weight_test_i <- gsDesign2:::get_combo_weight(rho[i], gamma[i], tau[i])
    info_wlr <- gsDesign2::gs_info_wlr(
      enroll_rate = enroll_rate,
      fail_rate = fail_rate,
      ratio = 1, # Experimental:Control randomization ratio
      event = NULL, # Events at analyses
      analysis_time = 30, # Times of analyses
      weight = eval(parse(text = weight_test_i)),
      approx = "asymptotic"
    )

    expect_equal(info_combo$info[i], info_wlr$info[1])
  }
})

# Test gs_prob_combo ----
## 1 analysis scenario ----
test_that("p efficacy", {

  lower <- -0.6
  upper <- 0.4
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)
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
  arm <- gs_create_arm(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    total_time = 1e6
  )
  sigma <- gsDesign2:::gs_sigma2_combo(
    arm0 = arm$arm0,
    arm1 = arm$arm1,
    tmax = 30,
    rho = rho,
    gamma = gamma,
    tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )
  corr <- cov2cor(sigma)
  n_test <- length(rho)
  theta <- rep(0, n_test)
  analysis <- 1

  prob <- gsDesign2:::gs_prob_combo(
    lower_bound = rep(lower, n_test),
    upper_bound = rep(upper, n_test),
    analysis = analysis,
    theta = theta,
    corr = corr,
    algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )

  p_efficacy <- gsDesign2:::pmvnorm_combo(
    lower = rep(upper, n_test),
    upper = rep(Inf, n_test),
    group = analysis,
    mean = theta,
    corr = corr
  )

  p_futility <- gsDesign2:::pmvnorm_combo(
    lower = rep(-Inf, n_test),
    upper = rep(lower, n_test),
    group = analysis,
    mean = theta,
    corr = corr
  )

  res <- list("prob" = prob, "p_efficacy" = p_efficacy, "p_futility" = p_futility)

  # p efficacy
  expect_equal(res$prob$probability[1], res$p_efficacy[1], tolerance = 0.001)

  # p futility
  expect_equal(res$prob$probability[2], res$p_futility[1], tolerance = 0.001)
})



## 2 analysis scenario ----
test_that("p efficacy1", {
  lower <- c(-0.2, -0.3)
  upper <- c(0.3, 0.4)
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)
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
  arm <- gs_create_arm(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    total_time = 1e6
  )
  sigma <- gsDesign2:::gs_sigma2_combo(
    arm0 = arm$arm0,
    arm1 = arm$arm1,
    tmax = 30,
    rho = rho,
    gamma = gamma,
    tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )
  corr <- cov2cor(sigma)
  n_test <- length(rho)
  theta <- rep(0, n_test)
  analysis <- c(1, 2)
  prob <- gsDesign2:::gs_prob_combo(
    lower_bound = rep(lower, n_test),
    upper_bound = rep(upper, n_test),
    analysis = analysis,
    theta = theta,
    corr = corr,
    algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )
  c <- c(1, 3)
  corr1 <- corr[c, c]
  p_efficacy_1 <- gsDesign2:::pmvnorm_combo(
    lower = rep(upper[1], 2),
    upper = rep(Inf, 2),
    group = 1,
    mean = theta[c],
    corr = corr1
  )
  p_futility_1 <- gsDesign2:::pmvnorm_combo(
    lower = rep(-Inf, 2),
    upper = rep(lower[1], 2),
    group = 1,
    mean = theta[c],
    corr = corr1
  )
  p_efficacy_2 <- gsDesign2:::pmvnorm_combo(
    lower = c(lower[1], upper[2]),
    upper = c(upper[1], Inf),
    group = analysis,
    mean = theta,
    corr = corr
  )
  p_futility_2 <- gsDesign2:::pmvnorm_combo(
    lower = c(lower[1], -Inf),
    upper = c(upper[1], lower[2]),
    group = analysis,
    mean = theta,
    corr = corr
  )

  res <- list(
    "prob" = prob,
    "p_efficacy_1" = p_efficacy_1,
    "p_efficacy_2" = p_efficacy_2,
    "p_futility_1" = p_futility_1,
    "p_futility_2" = p_futility_2
  )

  # p efficacy1
  expect_equal(res$prob$probability[1], res$p_efficacy_1[1], tolerance = 0.001)

  # p futility1
  expect_equal(res$prob$probability[3], res$p_futility_1[1], tolerance = 0.001)

  # p efficacy2
  expect_equal(res$prob$probability[2], res$p_efficacy_1[1] + res$p_efficacy_2[1], tolerance = 0.001)

  # p futility2
  expect_equal(res$prob$probability[4], res$p_futility_1[1] + res$p_futility_2[1], tolerance = 0.001)
})


# Test pmvnorm_combo ----
test_that("pmvnorm_combo calculate p for One test for all group or lower bound is -Inf.", {
  lower <- -Inf
  upper <- 0
  mean <- 0.3
  n_test <- 4
  rho <- c(1, 1, 0, 0)
  gamma <- c(0, 1, 0, 1)
  tau <- c(-1, -1, -1, -1)

  enroll_rate <- define_enroll_rate(
    stratum = "All",
    duration = c(2, 2, 10),
    rate = c(3, 6, 9)
  )
  fail_rate <- define_fail_rate(
    duration = c(3, 100),
    fail_rate = log(2) / c(9, 18),
    dropout_rate = rep(.001, 2),
    hr = c(.9, .6)
  )
  arm <- gs_create_arm(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    total_time = 1e6
  )
  sigma <- gsDesign2:::gs_sigma2_combo(
    arm0 = arm$arm0,
    arm1 = arm$arm1,
    tmax = 30,
    rho = rho,
    gamma = gamma,
    tau = rep(-1, length(rho)),
    approx = "asymptotic"
  )
  corr <- cov2cor(sigma)

  p <- gsDesign2:::pmvnorm_combo(
    lower = rep(lower, n_test),
    upper = rep(upper, n_test),
    group = 2,
    mean = rep(mean, n_test),
    corr = corr,
    algorithm = mvtnorm::GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )

  p_test <- mvtnorm::pmvnorm(
    lower = rep(lower, n_test),
    upper = rep(upper, n_test),
    mean = rep(mean, n_test),
    corr = corr,
    sigma = NULL,
    algorithm = mvtnorm::GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )

  res <- list("p" = p, "p_test" = p_test)

  p <- res$p
  p_test <- res$p_test

  expect_equal(p[1], p_test[1], tolerance = 0.001)
})

# Log-rank multiple analysis ----
## Test gs_utility_combo ----
test_that("gs_utility_combo output correct info as gs_info_combo", {
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
  analysis_time <- c(12, 24, 36)
  n_analysis <- length(analysis_time)
  fh_test <- rbind(data.frame(
    rho = 0,
    gamma = 0,
    tau = -1,
    test = 1,
    analysis = 1:3,
    analysis_time = analysis_time
  ))
  gs_arm <- gs_create_arm(
    enroll_rate,
    fail_rate,
    ratio = 1, # Randomization ratio
    total_time = max(analysis_time) # Total study duration
  )

  utility_combo <- gsDesign2:::gs_utility_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test = fh_test,
    ratio = 1,
    algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )

  info_combo_test <- gsDesign2:::gs_info_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    analysis_time = analysis_time,
    rho = 0,
    gamma = 0
  )

  res <- list(
    "n_analysis" = n_analysis,
    "utility_combo" = utility_combo,
    "info_combo_test" = info_combo_test
  )

  # gs_utility_combo output correct info as gs_info_combo
  expect_equal(res$utility_combo$info[1:11], res$info_combo_test[1:11])

  # gs_utility_combo output correct theta effect as gs_info_combo
  theta_test <- (-info_combo_test$delta) / sqrt(info_combo_test$sigma2)
  expect_equal(res$utility_combo$theta, theta_test)

  # gs_utility_combo output correct correlation matrix as gs_info_combo

  n_analysis <- res$n_analysis
  utility_combo <- res$utility_combo
  info_combo_test <- res$info_combo_test

  info <- info_combo_test[[10]]
  cov <- matrix(0, n_analysis, n_analysis)
  for (i in 1:n_analysis) {
    for (j in 1:n_analysis) {
      k <- min(i, j)
      cov[i, j] <- info[k] / (info[i] * info[j])
    }
  }
  corr_test <- cov2cor(cov)

  expect_equal(utility_combo$corr, corr_test)
})




## Multiple test analysis ----
test_that("gs_utility_combo output correct info as gs_info_combo", {
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
  analysis_time <- 36
  n_analysis <- length(analysis_time)
  rho <- c(0, 0.5, 1)
  gamma <- c(0.5, 0.5, 0.5)
  tau <- c(-1, -1, -1)
  fh_test <- rbind(data.frame(
    rho = rho,
    gamma = gamma,
    tau = tau,
    test = 1:3,
    analysis = 1,
    analysis_time = analysis_time
  ))
  gs_arm <- gs_create_arm(
    enroll_rate,
    fail_rate,
    ratio = 1, # Randomization ratio
    total_time = max(analysis_time) # Total study duration
  )

  utility_combo <- gsDesign2:::gs_utility_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    fh_test = fh_test,
    ratio = 1,
    algorithm = GenzBretz(maxpts = 1e5, abseps = 1e-5)
  )

  info_combo_test <- gsDesign2:::gs_info_combo(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    ratio = 1,
    analysis_time = analysis_time,
    rho = rho,
    gamma = gamma
  )

  res <- list(
    "rho" = rho,
    "gamma" = gamma,
    "tau" = tau,
    "analysis_time" = analysis_time,
    "n_analysis" = n_analysis,
    "gs_arm" = gs_arm,
    "utility_combo" = utility_combo,
    "info_combo_test" = info_combo_test
  )

  # gs_utility_combo output correct info as gs_info_combo
  utility_combo <- res$utility_combo
  info_combo_test <- res$info_combo_test
  expect_equal(utility_combo$info[1:11], info_combo_test[1:11])

  # gs_utility_combo output correct theta effect as gs_info_combo
  utility_combo <- res$utility_combo
  info_combo_test <- res$info_combo_test
  theta_test <- (-info_combo_test$delta) / sqrt(info_combo_test$sigma2)

  expect_equal(utility_combo$theta, theta_test)

  # gs_utility_combo output correct correlation matrix as gs_info_combo
  rho <- res$rho
  gamma <- res$gamma
  tau <- res$tau
  analysis_time <- res$analysis_time
  n_analysis <- res$n_analysis
  gs_arm <- res$gs_arm
  utility_combo <- res$utility_combo

  sigma2 <- gsDesign2:::gs_sigma2_combo(
    arm0 = gs_arm$arm0,
    arm1 = gs_arm$arm1,
    tmax = analysis_time,
    rho = rho,
    gamma = gamma,
    tau = tau
  )
  corr_test <- cov2cor(sigma2)

  expect_equal(utility_combo$corr, corr_test)
})

