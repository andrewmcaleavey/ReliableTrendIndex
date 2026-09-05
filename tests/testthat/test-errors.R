test_that("input validation works", {
  expect_error(se_slope_reliability(n = 1, sd = 5, r = 0.8), ">= 2")
  expect_error(se_slope_reliability(n = 5, sd = -1, r = 0.8), "positive")
  expect_error(se_slope_reliability(n = 5, sd = 5, r = 1.2), "\\[0, 1\\]")
  
  expect_error(rti(y = 1, sd = 5, r = 0.8), "length >= 2")
  expect_error(rti(c(1, 2), sd = -1, r = 0.8), "positive")
  expect_error(rti(c(1, 2), sd = 5, r = 1.5), "\\[0, 1\\]")
  expect_error(rti(c(1, 2), sd = c(5, 6, 7), r = 0.8), "length-y vector")
  expect_error(rti(c(1, 2), sem = function(time) c(1, 2, 3)), "length-y vector")
  expect_error(rti(c(1, 2), sem = 1, rc.type = "not-a-type"),
               "should be one of")
  expect_error(rti(c(1, 2), sd = 5, r = 0.8, t = c(1, 2, 3)), "same length")
  expect_error(rti(c(1, 2), sd = 5, r = 0.8, t = c(1, 1)), "Sxx = 0")

  expect_error(rci(1, sd1 = 5, sd2 = 8, r1 = 0.8, r2 = 1.2,
                   rc.type = "mcnemar"), "in \\[0, 1\\]")
  expect_error(rci(1, sd1 = -5, sd2 = 8, r1 = 0.8,
                   rc.type = "mcnemar"), "positive")
  expect_error(rci(1, sd1 = 5, sd2 = 8, r1 = 0.8,
                   rc.type = "not-a-type"), "should be one of")

  df <- data.frame(id = 1:2, time = 1:2, y = 1:2, sd = 5, r = 0.8)
  expect_error(rti_by(df, idd, time, y, sd, r), "Column `idd` not found")
  expect_error(rti_by(df, id, time, y, "notacol", r), "not found")
  expect_error(rti_by(df, id, time, y, sd, "notacol"), "not found")
})
