library(farsr)
context("Test farsr package")


test_that("make_filename check", {

  expect_equal(make_filename(2014), "accident_2014.csv.bz2")

})
