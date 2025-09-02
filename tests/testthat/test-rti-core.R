test_that("rti slope equals OLS slope with centered time", {
  set.seed(123)
  n <- 10
  t <- 1:n
  y <- 3 + 0.4 * (t - mean(t)) + rnorm(n, 0, 0.1)
  
  fit <- rti(y, sd = 8, r = 0.85, t = t)
  
  # manual OLS with centered t
  tc <- t - mean(t)
  Sxx <- sum(tc^2)
  beta1 <- sum(tc * y) / Sxx
  expect_equal(fit$estimate, beta1)
  
  # z, p, ci internally consistent
  expect_equal(fit$z, fit$estimate / fit$se)
  zcrit <- qnorm(0.975)
  expect_equal(fit$ci, c(fit$estimate - zcrit*fit$se, fit$estimate + zcrit*fit$se))
  expect_equal(fit$p, 2 * pnorm(-abs(fit$z)))
})

test_that("n = 2 works and matches hand calculation", {
  y <- c(10, 16)
  t <- c(1, 2)
  sd <- 6
  r <- 0.8
  
  fit <- rti(y, sd = sd, r = r, t = t)
  
  # slope with centered t is (y2 - y1) / (t2 - t1)
  slope <- (y[2] - y[1]) / (t[2] - t[1])
  Sxx <- 2 * (2^2 - 1) / 12  # 0.5
  se  <- sqrt(sd^2 * (1 - r) / Sxx)
  z   <- slope / se
  
  expect_equal(fit$estimate, slope)
  expect_equal(fit$se, se)
  expect_equal(fit$z, z)
})

test_that("plot returns a ggplot and band options are available", {
  y <- c(12, 11, 13, 16, 17, 19)
  fit <- rti(y, sd = 8, r = 0.85)
  
  p1 <- plot(fit, band = "slope")
  p2 <- plot(fit, band = "mean", include_intercept_uncertainty = TRUE)
  p3 <- plot(fit, band = "prediction")
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("rti_by accepts bare symbols bound to scalars for sd/r", {
  set.seed(1)
  df <- data.frame(
    id = rep(letters[1:2], each = 4),
    time = rep(1:4, 2),
    y = c(5 + 0.5*(1:4) + rnorm(4, 0, 2),
          3 - 0.3*(1:4) + rnorm(4, 0, 2))
  )
  sd_ext <- 8
  r_ext  <- 0.85
  out <- rti_by(df, id, time, y, sd = sd_ext, r = r_ext)
  expect_equal(nrow(out), 2L)
  expect_true(all(is.finite(out$z)))
})
