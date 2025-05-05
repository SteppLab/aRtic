# Test define_coord function

library(testthat)
library(tidyverse)
library(abind)
library(here)

test_that("define_coord rotates data", {
  test_data <- load_tsv(here("tests", "sample_data", "PLURAL44_BitePlane.tsv"))
  n_time <- dim(test_data[[1]])[1]
  n_sensors <- dim(test_data[[1]])[3]
  ref_idx <- c(1, 2, 3)
  bp_idx <- c(5, 6, 7)
  rotated_data <- define_coord(test_data, ref_idx, bp_idx)
  
  expect_equal(dim(rotated_data) [1], n_time)
  expect_equal(dim(rotated_data)[2], 3)
  expect_equal(dim(rotated_data)[3], n_sensors)
})

# Test Passed 5/5/2025