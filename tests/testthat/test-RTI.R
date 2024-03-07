# Tests for RTI.R (other than rti_calc_simple())
test_that("simple_rma() provides rma objects", {
  testthat::expect_true("rma" %in% class(simple_rma(15, 4.74^2)))
  testthat::expect_true("rma" %in% class(simple_rma(c(47.5, 32.5), 4.74^2)))
  testthat::expect_true("rma" %in% class(simple_rma(jt_example_data_1, 
                                          error_var = 4.74^2, 
                                          observed = "obs")))
})

# tests for compute_rti_data
# obsolete?

# tests for rti()
test_that("rti() works with a single numeric input", {
  expect_equal(rti(values = c(0, 15), sdiff = 4.74), 
               rti(values = 15, sdiff = 4.74))
})
