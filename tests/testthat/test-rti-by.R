test_that("rti_by matches per-id rti results (scalar sd/r)", {
  set.seed(42)
  df <- data.frame(
    id = rep(LETTERS[1:4], each = 6),
    time = rep(1:6, times = 4),
    y = c(
      5 + 0.7*(1:6) + rnorm(6, 0, 3),
      5 - 0.5*(1:6) + rnorm(6, 0, 3),
      3 + 0.0*(1:6) + rnorm(6, 0, 3),
      1 + 1.2*(1:6) + rnorm(6, 0, 3)
    )
  )
  
  out <- rti_by(df, id, time, y, sd = 8, r = 0.85)
  expect_equal(nrow(out), length(unique(df$id)))
  expect_true(all(c("id","n","estimate","se","z","p","ci_lower","ci_upper","fit") %in% names(out)))
  
  # compare to manual per-id
  by_manual <- lapply(split(df, df$id), function(d) rti(d$y, sd = 8, r = 0.85, t = d$time))
  ests <- vapply(by_manual, function(f) f$estimate, numeric(1))
  names(ests) <- names(by_manual)
  
  # order out by id
  out <- out[order(out$id), ]
  expect_equal(unname(ests), out$estimate)
})

test_that("rti_by supports sd/r as columns with per-id constants", {
  set.seed(99)
  df <- data.frame(
    id = rep(letters[1:3], each = 5),
    time = unlist(lapply(1:3, function(i) c(0, 1, 2, 4, 7))), # irregular spacing, same for each id
    y = c(
      2 + 0.5*c(0, 1, 2, 4, 7) + rnorm(5, 0, 2),
      4 - 0.4*c(0, 1, 2, 4, 7) + rnorm(5, 0, 2),
      1 + 0.0*c(0, 1, 2, 4, 7) + rnorm(5, 0, 2)
    )
  )
  # per-id constants repeated across rows
  sd_by_id <- setNames(c(6, 7, 8), letters[1:3])
  r_by_id  <- setNames(c(0.90, 0.80, 0.85), letters[1:3])
  df$sd <- sd_by_id[df$id]
  df$r  <- r_by_id[df$id]
  
  out <- rti_by(df, id, time, y, sd, r, na.rm = TRUE)
  expect_equal(nrow(out), 3L)
  
  # Cross-check one id manually
  d_b <- subset(df, id == "b")
  fit_b <- rti(d_b$y, sd = unique(d_b$sd), r = unique(d_b$r), t = d_b$time)
  expect_equal(out$estimate[out$id == "b"], fit_b$estimate)
})

test_that("rti_by errors when sd/r columns vary within id", {
  df <- data.frame(
    id = rep("x", 3),
    time = 1:3,
    y = c(0, 1, 2),
    sd = c(5, 6, 5),   # varies within id
    r = c(0.8, 0.8, 0.8)
  )
  expect_error(rti_by(df, id, time, y, sd, r), "constant within id")
})
