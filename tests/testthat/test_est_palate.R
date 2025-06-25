# Test est_palate

library(tidyverse)
library(zoo)
library(abind)
library(signal)
library(readr)
library(here)
library(pracma)

test_that("est_palate estimates the x,y,z coordinates of the palate from a palate trace", {
  ref_idx <- c(1, 2, 3)
  bp_idx <- c(5, 6, 7)
  pl_idx <- 8
  test_data <- load_tsv(here("tests", "sample_data", "PLURAL02_PalateTrace.tsv"))
  bite_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv")) 
  coord <- define_coord(bite_data[[1]], ref_idx, bp_idx)
  palate_data <- test_data[[1]]
  palate <- est_palate(palate_data, coord[[1]], ref_idx, pl_idx, coord[[2]], coord[[3]])
  
  
  # Check data output
  expect_true(is.data.frame(palate))
  expect_equal(ncol(palate), 3)
  
})

# Test Passed: 6/25/2025