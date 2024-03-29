test_eAccrual <- function(x, enroll_rate) {
  boundary <- cumsum(enroll_rate$duration)
  rate <- enroll_rate$rate
  xvals <- unique(c(x, boundary))

  eAc2 <- numeric(length(xvals))
  for (t in seq_along(xvals)) {
    val <- xvals[t]
    if (val <= boundary[1]) {
      eAc2[t] <- val * rate[1]
    } else if (val <= boundary[2]) {
      eAc2[t] <- boundary[1] * rate[1] + (val - boundary[1]) * rate[2]
    } else if (val <= boundary[3]) {
      eAc2[t] <- boundary[1] * rate[1] +
        (boundary[2] - boundary[1]) * rate[2] + (val - boundary[2]) * rate[3]
    } else {
      eAc2[t] <- boundary[1] * rate[1] +
        (boundary[2] - boundary[1]) * rate[2] + (boundary[3] - boundary[2]) * rate[3]
    }
  }

  ind <- !is.na(match(xvals, x))
  return(eAc2[ind])
}

testthat::test_that("expect_accrua doesn't match with the double programming e_Acurral function", {
  testthat::expect_equal(
    expected_accrual(
      time = 0:30,
      enroll_rate = define_enroll_rate(
        duration = c(3, 13, 18),
        rate = c(5, 20, 8)
      )
    ),
    test_eAccrual(
      x = 0:30,
      enroll_rate = define_enroll_rate(
        duration = c(3, 13, 18),
        rate = c(5, 20, 8)
      )
    )
  )
})

testthat::test_that("expect_accrual fail to identify a non-numerical input", {
  x <- c(0:20, "NA")
  expect_error(expect_message(
    expected_accrual(time = x),
    "gsDesign2: time in `expected_accrual()` must be a strictly increasing non-negative numeric vector"
  ))
})

testthat::test_that("expect_accrual fail to identify a negative input", {
  x <- -20:-1
  expect_error(expect_message(
    expected_accrual(time = x),
    "gsDesign2: time in `expected_accrual()` must be a strictly increasing non-negative numeric vector"
  ))
})

testthat::test_that("expect_accrual fail to identify a non-increasing input", {
  x <- 20:1
  expect_error(expect_message(
    expected_accrual(time = x),
    "gsDesign2: time in `expected_accrual()` must be a strictly increasing non-negative numeric vector"
  ))
})

# Add test cases for stratified design
testthat::test_that("expect_accrua fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 40,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, 33 * 30 * 2)
})

testthat::test_that("expect_accrua fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 33,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, 33 * 30 * 2)
})

testthat::test_that("expect_accrua fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 30,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, 30 * 30 * 2)
})

testthat::test_that("expect_accrua fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = 10,
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, 10 * 30 * 2)
})

testthat::test_that("expect_accrual fail to identify a non-dataframe input", {
  x <- expected_accrual(
    time = c(5, 10, 20, 33, 50),
    enroll_rate = define_enroll_rate(
      stratum = c("S1", "S2"),
      duration = 33,
      rate = c(30, 30)
    )
  )
  expect_equal(x, c(5, 10, 20, 33, 33) * 30 * 2)
})
