expect_deprecated_result <- function(code) {
  result <- NULL
  expect_warning(result <- eval.parent(substitute(code)), "deprecated")
  result
}

test_that("reliableTrend warns and preserves its historical result shape", {
  y <- c(10, 12, 11, 15)
  t <- c(0, 1, 3, 4)
  legacy <- expect_deprecated_result(reliableTrend(values = y, time = t, sem = 2))
  canonical <- rti(y = y, t = t, sem = 2)

  expect_s3_class(legacy, "reliableTrend")
  expect_false(any(c("sem", "sdiff") %in% names(legacy)))
  expect_equal(legacy[c("estimate", "intercept", "se", "z", "p", "ci",
                        "sigma2", "t", "t_centered", "y", "Sxx", "n")],
               canonical[c("estimate", "intercept", "se", "z", "p", "ci",
                           "sigma2", "t", "t_centered", "y", "Sxx", "n")])
})

test_that("legacy RCI wrappers warn and delegate to rci", {
  expected <- rci(difference = 6, sdiff = 3)
  expect_equal(expect_deprecated_result(rci_from_scores(x1 = 4, x2 = 10, sdiff = 3)), expected)
  expect_equal(expect_deprecated_result(jt_rci_calc(difference = 6, sdiff = 3)), expected)
})

test_that("rti_calc_simple preserves its legacy adapter fields", {
  result <- expect_deprecated_result(rti_calc_simple(c(3, 5, 7), sem = 1))
  direct <- rti(c(3, 5, 7), sem = 1)

  expect_s3_class(result, "rti_calc_simple")
  expect_named(result, c("rmaObj", "error_var", "fit"), ignore.order = FALSE)
  expect_equal(result$error_var, 1)
  expect_equal(result$rmaObj[c("estimate", "se", "z", "p")],
               direct[c("estimate", "se", "z", "p")])
  expect_identical(result$rmaObj, result$fit)
})

test_that("slope_se_reliability retains its class and agrees with rti", {
  y <- c(12, 11, 13, 16, 17)
  time <- c(0, 1, 2, 4, 7)
  legacy <- expect_deprecated_result(
    slope_se_reliability(y, r = 0.85, time = time, sd_single = 8)
  )
  direct <- rti(y, t = time, sd = 8, r = 0.85)

  expect_s3_class(legacy, "slopeSErel")
  expect_equal(legacy$slope_hat, direct$estimate)
  expect_equal(legacy$SE_reliability, direct$se)
  expect_equal(legacy$RTI, direct$z)
  expect_equal(legacy$intercept_hat, direct$intercept)
})

test_that("rti_by_person retains its legacy data-frame contract", {
  dat <- data.frame(
    id = rep(c("a", "b"), each = 3),
    time = rep(1:3, 2),
    score = c(1, 3, 5, 8, 6, 4)
  )
  legacy <- expect_deprecated_result(
    rti_by_person(dat, id, score, time, r = 0.8, sd_single = 5, verbose = TRUE)
  )
  direct <- rti_by(dat, id, time, score, sd = 5, r = 0.8)

  expect_named(legacy, c("id", "n", "S_xx", "slope_hat", "sd_single_used",
                          "sigma_e", "SE_reliability", "RTI", "crit", "RTI_cat"),
               ignore.order = FALSE)
  expect_equal(legacy$slope_hat, direct$estimate)
  expect_equal(legacy$SE_reliability, direct$se)
  expect_equal(legacy$RTI, direct$z)
})

test_that("rti_to_df retains its legacy columns", {
  fit <- rti(c(2, 4, 7), sem = 1)
  out <- expect_deprecated_result(rti_to_df(fit))

  expect_named(out, c("slope.est", "slope.lb", "slope.ub", "z", "p", "n",
                      "Sxx", "sigma2", "category.RTI", "category.RCI",
                      "pd.RTI", "pd.RCI"), ignore.order = FALSE)
  expect_equal(out$slope.est, fit$estimate)
  expect_equal(out$z, fit$z)
})
