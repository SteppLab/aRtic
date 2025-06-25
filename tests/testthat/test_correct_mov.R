# Testing correct_mov function

library(tidyverse)
library(zoo)
library(abind)
library(signal)
library(readr)
library(here)
library(pracma)

test_that("correct_mov rotates the 3D kinematic data and corrects for head movement", {
  test_data <- load_tsv(here("tests", "sample_data", "PLURAL02_RP.tsv"))
  data <- test_data[[1]]
  n_time <- dim(data)[1]
  n_sensors <- dim(data)[3]
  ref_idx <- c(1, 2, 3)
  bp_idx <- c(5, 6, 7)
  bite_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv")) 
  coord <- define_coord(bite_data[[1]], ref_idx, bp_idx)
  filtered <- interp_filter(data, ref_idx)
  corrected <- correct_mov(filtered, coord[[1]], ref_idx, coord[[2]], coord[[3]])
  
  # Check data output
  expect_true(is.array(corrected))
  expect_equal(dim(corrected), c(n_time, 3, n_sensors))
  
})

# Test Passed: 6/25/2025