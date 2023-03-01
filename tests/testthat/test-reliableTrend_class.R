

# reliableTrend()
# other tests cover the case when x is NULL
# this covers when it exists but is NOT an rmaObj
# need to expand to include that case
test_that("reliableTrend() works with non-standard input", {
  test_rmaObj <- readRDS(test_path("fixtures", 
                                   "mac_RTI_good.rds"))$rmaObj
  expect_true(is.reliableTrend(reliableTrend(x = list(RCI = 8))))
  expect_equal(8, reliableTrend(x = list(RCI = 8))$RCI)
  expect_equal(8, reliableTrend(x = list(RTI = 8))$RTI)
  expect_equal(8, reliableTrend(x = list(pd.RCI = 8))$pd.RCI)
  expect_equal(8, reliableTrend(x = list(pd.RTI = 8))$pd.RTI)
  expect_equal("Reliable Increase", reliableTrend(x = list(category.RCI = "Reliable Increase"))$category.RCI)
  expect_equal("Reliable Increase", reliableTrend(x = list(category.RTI = "Reliable Increase"))$category.RTI)
  expect_equal("Increase", reliableTrend(x = list(sign.RTI = "Increase"))$sign.RTI)
  expect_equal("Increase", reliableTrend(x = list(sign.difference = "Increase"))$sign.difference)
  # expect_equal(test_rmaObj, 
  #              reliableTrend(x = test_rmaObj$rmaObj)$rmaObj)
  expect_equal(8, reliableTrend(x = list(values = 8))$values)
  expect_equal(c(1,8), reliableTrend(x = list(values.prepost = c(1,8)))$values.prepost)
  expect_equal(8, reliableTrend(x = list(error_var = 8))$error_var)
  expect_equal(8, reliableTrend(x = list(cutpoint = 8))$cutpoint)
  expect_equal(8, reliableTrend(x = list(scale_RCI = 8))$scale_RCI)
})


# DONE

# print.reliableTrend(): 
#  Does it print something, does it return something
test_that("print.reliableTrend() works", {
  expect_true(is.reliableTrend(print(readRDS(test_path("fixtures", 
                                                       "mac_RTI_good.rds")))))
})

# summary.reliableTrend()
#   does it fail when it should, does it work when it should?
test_that("summary.reliableTrend works", {
  expect_warning(summary(readRDS(test_path("fixtures", "mac_RTI_NA.rds"))))
  expect_no_warning(summary(readRDS(test_path("fixtures", "mac_RTI_good.rds"))))
})

# validate_reliableTrend()
test_that("validate_reliableTrend() fails and works properly", {
  expect_error(validate_reliableTrend("Not a reliableTrend object"))
  expect_true(is.reliableTrend(validate_reliableTrend(new_reliableTrend())))
})

# new_reliableTrend()
test_that("new_reliableTrend() makes good objects", {
  expect_true(is.reliableTrend(new_reliableTrend()))
})

# is.reliableTrend()
test_that("is.reliableTrend() does the basics", {
  expect_true(is.reliableTrend(reliableTrend()))
  expect_false(is.reliableTrend("Nope."))
})

# rti_to_stripped_list()
#  rejects non-reliableTrend objects
#  returns a list
#  does not return a reliableTrend object. 
test_that("rti_to_stripped_list() does the basics", {
  expect_error(rti_to_stripped_list(45))
  expect_true(is.list(rti_to_stripped_list(reliableTrend())))
  expect_false(is.reliableTrend(rti_to_stripped_list(reliableTrend())))
})
# rti_to_df()
test_that("rti_to_df works", {
  expect_true(is.data.frame(rti_to_df(readRDS(test_path("fixtures", 
                                                        "mac_RTI_good.rds")))))
  expect_warning(is.data.frame(rti_to_df(rti(mac_height$obs, scale_rci = 1.39))))
})

