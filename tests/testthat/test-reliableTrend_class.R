test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
# need to test these: 

# validate_reliableTrend()
test_that("validate_reliableTrend() fails and works properly", {
  expect_error(validate_reliableTrend("Not a reliableTrend object"))
  expect_true(is.reliableTrend(validate_reliableTrend(new_reliableTrend())))
})
# reliableTrend()

# and these generic functions: 
# summary.reliableTrend()
#   does it fail when it should, does it work when it should?
# print.reliableTrend(): 
#  Does it print something, does it return something


# DONE

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
  expect_true(is.data.frame(rti_to_df(suppressWarnings(rti(mac_height$obs, 
                                                           scale_rci = 1.39)))))
  expect_warning(is.data.frame(rti_to_df(rti(mac_height$obs, scale_rci = 1.39))))
})

