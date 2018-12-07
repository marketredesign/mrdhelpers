context("checkExtension")


test_that("Check for inputs", {
  expect_error(checkExtension())
  expect_error(checkExtension(file = "testdata/test1.csv"))
  expect_error(checkExtension(exp_ext = ".csv"))
  expect_true(checkExtension(file = "testdata/test1.csv", exp_ext = ".csv"))
  expect_true(checkExtension(file = "testdata/test1.csv", exp_ext = "csv"))
  expect_false(checkExtension(file = "testdata/test1.csv", exp_ext = "xlsx"))
})



