test_that("is.reliableTrend works on positives and negatives", {
  fit <- rti(c(10, 12, 13), sd = 6, r = 0.85)
  expect_true(is.reliableTrend(fit))
  expect_false(is.reliableTrend(list()))
  expect_false(is.reliableTrend(1:3))
})

test_that("is.reliableTrend is a scalar logical", {
  fit <- rti(c(5, 6), sd = 4, r = 0.9)
  expect_type(is.reliableTrend(fit), "logical")
  expect_length(is.reliableTrend(fit), 1L)
})
