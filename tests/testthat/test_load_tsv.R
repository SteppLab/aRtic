# Test load_tsv function

# Purpose: To test the functionality of the load_tsv 

library(testthat)
source(".\\R\\load_tsv.R")


test_that("load_tsv returns expected structure", {
  result <- load_tsv(".\\tests\\sample_data\\PLURAL44_BitePlane.tsv")
  expect_type(result, "list")
  expect_length(result, 2)
  
  expect_true(is.array(result$data))
  expect_equal(length(dim(result$data)), 3)
  
  expect_type(result$n_time, "double")
  expect_equal(length(result$n_time), dim(result$data)[1])
})

# Test Passed 5/4/2025
