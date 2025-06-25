# Test define_coord function

library(testthat)
library(tidyverse)
library(abind)
library(here)

test_that("define_coord rotates data", {
  test_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv"))
  n_time <- dim(test_data[[1]])[1]
  n_sensors <- dim(test_data[[1]])[3]
  ref_idx <- c(1, 2, 3)
  bp_idx <- c(5, 6, 7)
  rotated <- define_coord(test_data[[1]], ref_idx, bp_idx)
  
  # Check list structure
  expect_type(rotated, "list")
  expect_length(rotated, 3)
  
  # Check rotated_data
  expect_true(is.array(rotated[[1]]))
  expect_equal(length(dim(rotated[[1]])), 3)
  expect_equal(dim(rotated[[1]]), c(n_time, 3, n_sensors))
  
  # Check base_rt
  expect_true(is.matrix(rotated[[2]]))
  expect_equal(dim(rotated[[2]]), c(3,3))
  
  # Check base_center
  expect_true(is.numeric(rotated[[3]]))
  expect_equal(length(rotated[[3]]), 3)
  
})

# Test Passed 6/25/2025