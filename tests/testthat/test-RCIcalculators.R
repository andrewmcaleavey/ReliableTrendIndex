test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
#' Test correct input distinctions (e.g. t1 but  not t2, diff without sdiff info)

# rci_calc_jt(.8, 10) # minimum needed is reliability and SD
# rci_calc_jt() # error
# rci_calc_jt(.8, 10, .2) # uses sdiff, reliability ignored
# rci_calc_jt(.8, 10, .2, .5) # uses sdiff value
# rci_calc_jt(c(15))
# rci_calc_jt(.8, 10, sem = .3)


#' Test proper calculation based on established benchmark in 1991 paper.
testthat::test_that("recaptures J&T example RCI only", {
  computed_rci <- rci_calc_jt(sdiff = 4.74)
  difference <- 15
  expect_equal(rci_calc_jt(sdiff = 4.74), 
               9.29, 
               tolerance = .001)
  expect_equal(15 / rci_calc_jt(sdiff = 4.74, verbose = TRUE)$sdiff, 
               3.16, 
               tolerance = .009)
  expect_equal(rci_calc_jt(sd1 = 7.5, 
                           rxx = .8), 
               rci_calc_jt(sdiff = 4.74), 
               tolerance = .007)
})
