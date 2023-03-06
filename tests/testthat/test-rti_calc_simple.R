test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("rti_calc_simple() returns reliableTrend objects", {
  #' The important cases to test: 
  #' one value
  #'   rti_calc_simple(values = c(15), variance = 4.74^2)
  expect_reliableTrend(rti_calc_simple(values = c(15), variance = 4.74^2))
  #' two values
  #'   rti_calc_simple(values = c(47.5, 32.5), variance = 3.35^2)
  expect_reliableTrend(rti_calc_simple(values = c(47.5, 32.5), variance = 4.74^2))
  
  #' three values
  #' should throw a warning about even spacing
  expect_reliableTrend(suppressWarnings(rti_calc_simple(values = c(47.5, 45, 32.5), 
                                       variance = 3.35^2, 
                                       fixIntWhen3 = TRUE)))
  expect_reliableTrend(suppressWarnings(rti_calc_simple(values = c(47.5, 45, 32.5), 
                                                        variance = 3.35^2, 
                                                        fixIntWhen3 = FALSE)))
  #' four or more values
  #' should throw a warning about even spacing
  expect_reliableTrend(suppressWarnings(rti_calc_simple(values = c(47.5, 45, 36, 32.5), 
                                                        variance = 3.35^2)))
})

