# Testing interp_filter function

library(tidyverse)
library(zoo)
library(abind)
library(signal)
library(readr)
library(here)

test_that("interp_filter filters data", {
  test_data <- load_tsv(here("tests", "sample_data", "PLURAL44_RP.tsv"))
  n_time <- dim(test_data[[1]])[1]
  n_sensors <- dim(test_data[[1]])[3]
  ref_idx <- c(1, 2, 3)
  
  filtered <- interp_filter(test_data, ref_idx)
  
  expect_equal(dim(filtered), dim(test_data[[1]]))
})

# Test Passed 5/6/2025