# Test rotate

library(tidyverse)
library(zoo)
library(abind)
library(signal)
library(readr)
library(here)
library(pracma)

test_that("The rotate function aligns the sensor recording with the coordinate plane", {
  ref_idx <- c(1, 2, 3)
  bp_idx <- c(5, 6, 7)
  test_data <- load_tsv(here("tests", "sample_data", "PLURAL02_RP.tsv"))
  bite_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv")) 
  coord <- define_coord(bite_data[[1]], ref_idx, bp_idx)
  corrected <- rotate(test_data[[1]], coord[[1]], ref_idx, coord[[2]], coord[[3]], test_data[[2]])
  
  
  # Check data output
  expect_true(is.data.frame(corrected))
  expect_equal(nrow(corrected), length(test_data[[2]])*dim(test_data[[1]])[3])
  
})

# Test Passed: 1/2/2025