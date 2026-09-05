test_that("rti has a stable hand-calculated result contract", {
  y <- c(10, 12, 15, 16)
  t <- c(0, 1, 3, 4)
  sd_ext <- 5
  r_ext <- 0.84

  fit <- rti(y = y, t = t, sd = sd_ext, r = r_ext)
  tc <- t - mean(t)
  expected_sxx <- sum(tc^2)
  expected_slope <- sum(tc * y) / expected_sxx
  expected_sigma2 <- sd_ext^2 * (1 - r_ext)
  expected_se <- sqrt(expected_sigma2 / expected_sxx)

  expect_s3_class(fit, "reliableTrend")
  expect_named(fit, c("estimate", "intercept", "se", "z", "p", "ci",
                      "sigma2", "t", "t_centered", "y", "Sxx", "n",
                      "sd", "r", "sem", "sdiff", "level", "rc.type", "call"),
               ignore.order = FALSE)
  expect_equal(fit$estimate, expected_slope)
  expect_equal(fit$intercept, mean(y))
  expect_equal(fit$Sxx, expected_sxx)
  expect_equal(fit$sigma2, expected_sigma2)
  expect_equal(fit$se, expected_se)
  expect_equal(fit$z, expected_slope / expected_se)
  expect_equal(fit$p, 2 * pnorm(-abs(fit$z)))
})

test_that("rti measurement-error parameterizations are equivalent", {
  y <- c(8, 9, 11, 12, 15)
  t <- c(0, 1, 2, 4, 7)
  sd_ext <- 10
  r_ext <- 0.91
  sem <- sd_ext * sqrt(1 - r_ext)
  sdiff <- sqrt(2) * sem

  from_sd_r <- rti(y, sd = sd_ext, r = r_ext, t = t)
  from_sem <- rti(y, sem = sem, t = t)
  from_sdiff <- rti(y, sdiff = sdiff, t = t)

  expect_equal(from_sd_r[c("estimate", "se", "z", "p", "ci", "sigma2")],
               from_sem[c("estimate", "se", "z", "p", "ci", "sigma2")])
  expect_equal(from_sd_r[c("estimate", "se", "z", "p", "ci", "sigma2")],
               from_sdiff[c("estimate", "se", "z", "p", "ci", "sigma2")])
  expect_true(is.na(from_sem$sd))
  expect_true(is.na(from_sdiff$sem))
})

test_that("rti accepts static or time-varying measurement-error definitions", {
  y <- c(8, 10, 13)
  t <- c(0, 2, 5)
  fit <- rti(y, t = t, sd = 10, r = 0.8)

  expect_equal(fit$sigma2, 10^2 * (1 - 0.8))

  sd_i <- c(10, 12, 14)
  r_i <- c(0.8, 0.85, 0.9)
  varying <- rti(y, t = t, sd = sd_i, r = r_i)
  tc <- t - mean(t)
  expected_se <- sqrt(sum(tc^2 * sd_i^2 * (1 - r_i)) / sum(tc^2)^2)
  expect_equal(varying$se, expected_se)
  expect_equal(varying$sigma2, sd_i^2 * (1 - r_i))

  functional <- rti(y, t = t, sem = function(time) 1 + time / 10)
  sem_i <- 1 + t / 10
  expect_equal(functional$se,
               sqrt(sum(tc^2 * sem_i^2) / sum(tc^2)^2))
})

test_that("rti accepts every rc.type with variable error values", {
  y <- c(8, 10, 13)
  t <- c(0, 2, 5)
  sd_i <- c(10, 12, 14)
  r_i <- c(0.80, 0.85, 0.90)

  fits <- lapply(c("jt", "maassen", "mcnemar"), function(rc.type) {
    rti(y, t = t, sd = sd_i, r = r_i, rc.type = rc.type)
  })

  expect_equal(vapply(fits, `[[`, character(1), "rc.type"),
               c("jt", "maassen", "mcnemar"))
  expect_true(all(vapply(fits, function(fit) is.finite(fit$z), logical(1))))
  expect_equal(fits[[1]]$se, fits[[2]]$se)
  expect_equal(fits[[2]]$se, fits[[3]]$se)
})

test_that("two-point rti is the corresponding RCI statistic", {
  y <- c(21, 28)
  sd_ext <- 9
  r_ext <- 0.75
  sdiff <- sd_ext * sqrt(2 * (1 - r_ext))

  fit <- rti(y, sd = sd_ext, r = r_ext)
  expect_equal(fit$z, rci(difference = y[2] - y[1], sdiff = sdiff))
  expect_equal(fit$se, sdiff)
})

test_that("two-point rti and mcnemar rci agree with varying errors", {
  y <- c(21, 28)
  difference <- y[2] - y[1]
  sd_i <- c(9, 12)
  r_i <- c(0.75, 0.90)

  fit <- rti(y, t = c(1, 2), sd = sd_i, r = r_i)
  rci_value <- rci(difference, sd1 = sd_i[1], sd2 = sd_i[2],
                   r1 = r_i[1], r2 = r_i[2], rc.type = "mcnemar")
  sdiff <- sqrt(sd_i[1]^2 * (1 - r_i[1]) +
                sd_i[2]^2 * (1 - r_i[2]))

  expect_equal(fit$estimate, difference)
  expect_equal(fit$se, sdiff)
  expect_equal(fit$z, rci_value)
})

test_that("two-point rti and maassen rci agree with varying SDs", {
  y <- c(21, 28)
  difference <- y[2] - y[1]
  sd_i <- c(9, 12)
  r_common <- 0.80

  fit <- rti(y, t = c(1, 2), sd = sd_i, r = r_common)
  rci_value <- rci(difference, sd1 = sd_i[1], sd2 = sd_i[2],
                   r1 = r_common, rc.type = "maassen")
  sdiff <- sqrt((sd_i[1]^2 + sd_i[2]^2) * (1 - r_common))

  expect_equal(fit$estimate, difference)
  expect_equal(fit$se, sdiff)
  expect_equal(fit$z, rci_value)
})

test_that("rci has stable primary and derived input paths", {
  difference <- c(-4, 0, 6)
  sd_ext <- 8
  r_ext <- 0.80
  sem <- sd_ext * sqrt(1 - r_ext)
  sdiff <- sqrt(2) * sem

  direct <- rci(difference = difference, sdiff = sdiff)
  from_scores <- rci(t1 = c(10, 10, 10), t2 = c(6, 10, 16), sdiff = sdiff)
  from_sem <- rci(difference = difference, sem = sem)
  from_sd_r <- rci(difference = difference, sd1 = sd_ext, r1 = r_ext)
  details <- rci(difference = difference, sdiff = sdiff, verbose = TRUE)

  expect_equal(direct, difference / sdiff)
  expect_equal(from_scores, direct)
  expect_equal(from_sem, direct)
  expect_equal(from_sd_r, direct)
  expect_named(details, c("RCI", "difference", "scale_rci", "sdiff", "sem",
                          "r1", "r2", "sd1", "sd2", "prob", "rc.type"),
               ignore.order = FALSE)
  expect_equal(details$RCI, direct)
  expect_equal(details$scale_rci, qnorm(0.975) * sdiff)
})

test_that("rci preserves documented heteroscedastic formulae", {
  difference <- 5
  maassen_sdiff <- sqrt((6^2 + 8^2) * (1 - 0.75))
  mcnemar_sdiff <- sqrt(6^2 * (1 - 0.75) + 8^2 * (1 - 0.85))

  expect_equal(rci(difference, sd1 = 6, sd2 = 8, r1 = 0.75,
                   rc.type = "maassen"), difference / maassen_sdiff)
  expect_equal(rci(difference, sd1 = 6, sd2 = 8, r1 = 0.75, r2 = 0.85,
                   rc.type = "mcnemar"), difference / mcnemar_sdiff)
})

test_that("rci allows different errors at the two occasions", {
  difference <- c(-3, 4, 9)
  sd1 <- 5
  sd2 <- 11
  r1 <- 0.70
  r2 <- 0.92
  sdiff <- sqrt(sd1^2 * (1 - r1) + sd2^2 * (1 - r2))

  fit <- rci(difference, sd1 = sd1, sd2 = sd2, r1 = r1, r2 = r2,
             rc.type = "mcnemar", verbose = TRUE)

  expect_equal(fit$sdiff, sdiff)
  expect_equal(fit$RCI, difference / sdiff)
  expect_equal(fit[c("sd1", "sd2", "r1", "r2")],
               list(sd1 = sd1, sd2 = sd2, r1 = r1, r2 = r2))
})

test_that("rti_by is a stable grouped wrapper around rti", {
  dat <- data.frame(
    person = rep(c("a", "b"), each = 4),
    visit = rep(c(0, 1, 3, 6), 2),
    score = c(10, 11, 14, 16, 20, 19, 17, 15)
  )
  out <- rti_by(dat, person, visit, score, sd = 7, r = 0.8)
  manual_a <- rti(dat$score[dat$person == "a"], t = dat$visit[dat$person == "a"],
                  sd = 7, r = 0.8)
  manual_b <- rti(dat$score[dat$person == "b"], t = dat$visit[dat$person == "b"],
                  sd = 7, r = 0.8)

  expect_named(out, c("id", "n", "estimate", "se", "z", "p", "ci_lower",
                      "ci_upper", "intercept", "Sxx", "sigma2", "level", "fit"),
               ignore.order = FALSE)
  expect_s3_class(out$fit[[1]], "reliableTrend")
  expect_equal(out$estimate, c(manual_a$estimate, manual_b$estimate))
  expect_equal(out$se, c(manual_a$se, manual_b$se))
  expect_equal(out$fit[[1]]$ci, c(out$ci_lower[1], out$ci_upper[1]))
})

test_that("canonical functions retain their public validation behavior", {
  expect_error(rti(c(1, 2), sd = 3, r = 0.8, t = c(1, 1)), "Sxx = 0")
  expect_error(rci(difference = 1), "Unable to compute `sdiff`")
  expect_error(rci(difference = 1, sdiff = 0), "positive")
  expect_error(rti_by(list(), id, time, y, sd = 2, r = 0.8), "data.frame")
})
