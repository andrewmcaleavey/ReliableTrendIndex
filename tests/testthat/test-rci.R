# tests/testthat/test-rci.R

test_that("rci: computes from difference and sdiff, vectorized", {
  diff <- c(0, 1.96, -1.96, 3.2)
  out  <- rci(difference = diff, sdiff = 1)
  expect_type(out, "double")
  expect_length(out, length(diff))
  expect_equal(out, diff, tolerance = 1e-12)
})

test_that("rci: derives difference from t1/t2 and warns on recycling", {
  t1 <- c(1, 2, 3, 4)
  t2 <- c(3, 5)        # shorter -> recycle
  expect_warning(
    out <- rci(t1 = t1, t2 = t2, sdiff = 2),
    "lengths differ; recycling will be applied"
  )
  expect_equal(out, (t2 - t1) / 2)  # base R recycling
})

test_that("rci: JT via sem equals JT via sd1 + r1", {
  sd1 <- 10
  r1  <- 0.75
  sem <- sd1 * sqrt(1 - r1)
  diff <- 5
  
  # via sem
  out_sem <- rci(difference = diff, sem = sem)
  # via sd1 + r1
  out_sdr <- rci(difference = diff, sd1 = sd1, r1 = r1)
  
  sdiff <- sqrt(2) * sem
  expect_equal(out_sem, diff / sdiff, tolerance = 1e-12)
  expect_equal(out_sdr, out_sem,     tolerance = 1e-12)
})

test_that("rci: Maassen and McNemar sdiff formulas", {
  sd1 <- 12; sd2 <- 9; r1 <- 0.8; r2 <- 0.65
  diff <- 6
  
  # Maassen: sqrt((sd1^2 + sd2^2) * (1 - r1))
  sdiff_maassen <- sqrt((sd1^2 + sd2^2) * (1 - r1))
  out_maassen   <- rci(difference = diff, rc.type = "maassen", sd1 = sd1, sd2 = sd2, r1 = r1)
  expect_equal(out_maassen, diff / sdiff_maassen, tolerance = 1e-12)
  
  # McNemar: sqrt(sd1^2 * (1 - r1) + sd2^2 * (1 - r2))
  sdiff_mcnemar <- sqrt(sd1^2 * (1 - r1) + sd2^2 * (1 - r2))
  out_mcnemar   <- rci(difference = diff, rc.type = "mcnemar", sd1 = sd1, sd2 = sd2, r1 = r1, r2 = r2)
  expect_equal(out_mcnemar, diff / sdiff_mcnemar, tolerance = 1e-12)
})

test_that("rci: Maassen/McNemar error when required args missing or non-numeric", {
  expect_error(rci(difference = 1, rc.type = "maassen", sd1 = 1, sd2 = 1), "requires `sd1`, `sd2`, and `r1`")
  expect_error(rci(difference = 1, rc.type = "mcnemar", sd1 = 1, sd2 = 1, r1 = 0.8), "requires `sd1`, `sd2`, `r1`, and `r2`")
  expect_error(rci(difference = 1, rc.type = "maassen", sd1 = "a", sd2 = 1, r1 = 0.9), "`sd1`, `sd2`, and `r1` must be numeric")
  expect_error(rci(difference = 1, rc.type = "mcnemar", sd1 = 1, sd2 = 1, r1 = 0.9, r2 = "b"), "`sd1`, `sd2`, `r1`, and `r2` must be numeric")
})

test_that("rci: scale_rci path works (no sdiff, sem, or sd+r provided)", {
  diff <- 2
  prob <- 0.975
  z    <- stats::qnorm(prob)
  scale_rci <- z * 2  # implies sdiff = 2
  out  <- rci(difference = diff, scale_rci = scale_rci, prob = prob)
  expect_equal(out, diff / 2, tolerance = 1e-12)
  
  # with another prob
  prob2 <- 0.84
  z2    <- stats::qnorm(prob2)
  scale_rci2 <- z2 * 3.5  # sdiff = 3.5
  out2  <- rci(difference = diff, scale_rci = scale_rci2, prob = prob2)
  expect_equal(out2, diff / 3.5, tolerance = 1e-12)
})

test_that("rci: helper scale_rci_calc() fallback is used if present", {
  # define a temporary helper in the global env; ensure cleanup
  helper <- function(sdiff, rxx, sd1, sem, prob, verbose) stats::qnorm(prob) * 3
  assign("scale_rci_calc", helper, envir = .GlobalEnv)
  on.exit({ if (exists("scale_rci_calc", envir = .GlobalEnv)) rm(scale_rci_calc, envir = .GlobalEnv) }, add = TRUE)
  
  # With no sdiff/sem/sd+r/scale_rci, fallback should set scale_rci = z * 3 -> sdiff = 3
  out <- rci(difference = 3, prob = 0.975)
  expect_equal(out, 3 / 3, tolerance = 1e-12)
})

test_that("rci: prob validation", {
  expect_error(rci(difference = 1, sdiff = 1, prob = 0),     "must be a single number in \\(0, 1\\)")
  expect_error(rci(difference = 1, sdiff = 1, prob = 1),     "must be a single number in \\(0, 1\\)")
  expect_error(rci(difference = 1, sdiff = 1, prob = -0.1),  "must be a single number in \\(0, 1\\)")
  expect_error(rci(difference = 1, sdiff = 1, prob = NA_real_), "must be a single number in \\(0, 1\\)")
})

test_that("rci: input validation for difference/t1/t2/sem/sdiff/r1", {
  # need difference OR both t1 & t2
  expect_error(rci(t1 = 1, sdiff = 1), "Provide either `difference` or both `t1` and `t2`")
  expect_error(rci(t2 = 1, sdiff = 1), "Provide either `difference` or both `t1` and `t2`")
  
  # non-numeric difference / t1/t2
  expect_error(rci(difference = "a", sdiff = 1), "`difference` must be numeric")
  expect_error(rci(t1 = "a", t2 = 2, sdiff = 1), "`t1` and `t2` must be numeric")
  
  # sem must be positive numeric
  expect_error(rci(difference = 1, sem = -1), "`sem` must be positive numeric")
  expect_error(rci(difference = 1, sem = NA_real_), "`sem` must be positive numeric")
  
  # sdiff must be positive numeric
  expect_error(rci(difference = 1, sdiff = 0),  "`sdiff` must be a positive numeric value")
  expect_error(rci(difference = 1, sdiff = -1), "`sdiff` must be a positive numeric value")
  
  # r1 range
  expect_error(rci(difference = 1, sd1 = 10, r1 = 1.2), "`r1` must be in \\[0, 1\\]")
  expect_error(rci(difference = 1, sd1 = 10, r1 = -0.1), "`r1` must be in \\[0, 1\\]")
  
  # rc.type choices
  expect_error(rci(difference = 1, sdiff = 1, rc.type = "not-a-type"), "'arg' should be one of")
})

test_that("rci: verbose returns a well-formed list with consistent internals", {
  sd1 <- 8; r1 <- 0.6; diff <- c(2, -3, 4.5); prob <- 0.975
  res <- rci(difference = diff, sd1 = sd1, r1 = r1, prob = prob, verbose = TRUE)
  
  expect_type(res, "list")
  expect_true(all(c("RCI","difference","scale_rci","sdiff","sem","r1","sd1","prob","rc.type") %in% names(res)))
  
  # internals are consistent:
  expect_equal(res$RCI, diff / res$sdiff, tolerance = 1e-12)
  expect_equal(res$scale_rci, stats::qnorm(prob) * res$sdiff, tolerance = 1e-12)
  expect_equal(res$sem, sd1 * sqrt(1 - r1), tolerance = 1e-12)
})

test_that("rci: provided sdiff overrides rc.type and other derivations", {
  diff <- c(4, -2)
  sdiff <- 2
  out_jt      <- rci(difference = diff, sdiff = sdiff, rc.type = "jt")
  out_maassen <- rci(difference = diff, sdiff = sdiff, rc.type = "maassen", sd1 = 10, sd2 = 9, r1 = 0.8)
  out_mcnemar <- rci(difference = diff, sdiff = sdiff, rc.type = "mcnemar", sd1 = 10, sd2 = 9, r1 = 0.8, r2 = 0.7)
  
  expect_equal(out_jt,      diff / sdiff, tolerance = 1e-12)
  expect_equal(out_maassen, diff / sdiff, tolerance = 1e-12)
  expect_equal(out_mcnemar, diff / sdiff, tolerance = 1e-12)
})
