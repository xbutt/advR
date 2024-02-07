library(testthat)
library(advR)


#testing if import_nc returns a list
test_that("process_nc returns a list", {

  result <- process_nc(directory = "./data",
                       pattern = ".nc",
                       cell_ids = c(327, 328, 329))

  expect_is(result, "list")
})


#testing if process_nc handles invalid directory
test_that("process_nc handles invalid directory", {

  invalid_result <- process_nc(directory = "./invalid_directory", pattern = ".nc", cell_ids = c(327, 328, 329))

  expect_is(invalid_result, "NULL")
})

