test_that("Sxx closed form matches computed and SE agrees", {
  for (n in 2:12) {
    t <- 1:n
    Sxx_direct <- sum( (t - mean(t))^2 )
    Sxx_closed <- n * (n^2 - 1) / 12
    expect_equal(Sxx_direct, Sxx_closed)
    
    se_eq <- se_slope_reliability(n = n, sd = 10, r = 0.8, spacing = "equal")
    se_cu <- se_slope_reliability(n = n, sd = 10, r = 0.8, spacing = "custom", t = t)
    expect_equal(se_eq, se_cu)
  }
})

test_that("custom spacing uses Sxx based on t", {
  t1 <- c(0, 1, 2, 4, 7)
  t2 <- 1:5
  expect_true(sum( (t1 - mean(t1))^2 ) != sum( (t2 - mean(t2))^2 ))
  
  se1 <- se_slope_reliability(n = length(t1), sd = 5, r = 0.9, spacing = "custom", t = t1)
  se2 <- se_slope_reliability(n = length(t2), sd = 5, r = 0.9, spacing = "custom", t = t2)
  expect_true(se1 != se2)
})
