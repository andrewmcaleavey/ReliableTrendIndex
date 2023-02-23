test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
# need to test these: 
# new_reliableTrend()
# validate_reliableTrend()
# reliableTrend()

# and these generic functions: 
# summary.reliableTrend()
# print.reliableTrend()
# rti_to_stripped_list()
# rti_to_df()
# is.reliableTrend()
test_that("is.reliableTrend() does the basics", {
  expect_true(is.reliableTrend(reliableTrend()))
  expect_false(is.reliableTrend("Nope."))
})
