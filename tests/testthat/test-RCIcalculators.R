test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
#' Test correct input distinctions (e.g. t1 but  not t2, diff without sdiff info)

# scale_rci_calc(.8, 10) # minimum needed is reliability and SD
# scale_rci_calc() # error
# scale_rci_calc(.8, 10, .2) # uses sdiff, reliability ignored
# scale_rci_calc(.8, 10, .2, .5) # uses sdiff value
# scale_rci_calc(c(15))
# scale_rci_calc(.8, 10, sem = .3)


#' Test proper calculation based on established benchmark in 1991 paper.
testthat::test_that("scale_rci_calc() recaptures J&T example", {
  computed_rci <- scale_rci_calc(sdiff = 4.74)
  difference <- 15
  expect_equal(scale_rci_calc(sdiff = 4.74), 
               9.29, 
               tolerance = .001)
  expect_equal(15 / scale_rci_calc(sdiff = 4.74, verbose = TRUE)$sdiff, 
               3.16, 
               tolerance = .009)
  expect_equal(scale_rci_calc(sd1 = 7.5, 
                           rxx = .8), 
               scale_rci_calc(sdiff = 4.74), 
               tolerance = .007)
})


#' testing the score RCI
test_that("jt_rci_calc() recaptures J&T example", {
  expect_equal(jt_rci_calc(difference = 15, sdiff = 4.74), 
               3.16, 
               tolerance = .006)
  expect_equal(jt_rci_calc(difference = 15, sem = 3.35), 
               3.16, 
               tolerance = .006)
  expect_equal(jt_rci_calc(difference = 15, scale_rci = scale_rci_calc(sdiff = 4.74)), 
               3.16, 
               tolerance = .006)
  expect_equal(jt_rci_calc(t1 = 32.5, t2 = 47.5, sdiff = 4.74), 
               3.16, 
               tolerance = .006)
  expect_equal(jt_rci_calc(t1 = c(32.5, 31.5), t2 = c(47.5, 48), sdiff = 4.74)[1], 
               3.16, 
               tolerance = .006)
})

test_that("jt_rci_calc() throws a warning when you want it", {
  expect_warning(jt_rci_calc(t1 = c(32.5, 31.5), t2 = 47.5, sdiff = 4.74))
})

# warning condition
