# testing helper file

# helper to check something is of class reliableTrend
expect_reliableTrend <- function(x){
  expect_true(is.reliableTrend(x))
}