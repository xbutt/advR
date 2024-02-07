library(testthat)
library(advR)

#creating sample data for testing
sample_data <- process_nc(cell_ids = c(327, 328, 329))


test_that("idf function calculates IDF quantiles and generates plot", {
  #checking if the output is a list with 'data' and 'plot' elements
  expect_that(idf(x_list = sample_data)$data, is_a("data.table"))
  expect_that(idf(x_list = sample_data)$plot, is_a("ggplot"))

  #checking if the output data table has the expected columns
  expect_equal(colnames(idf(x_list = sample_data)$data), c("cell_id", "dur", "rp", "value"))

  #checking if the generated plot is not NULL
  expect_false(is.null(idf(x_list = sample_data)$plot))
})
