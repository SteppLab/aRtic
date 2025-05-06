# Correct Movement Function

library(tidyverse)


correct_mov <- function(coord, filtered, ref_idx, bp_idx) {
  
  n_time <- dim(filtered)[1]
  n_dims <- dim(filtered)[2]
  n_sens <- dim(filtered)[3]
  
  for (k in 1:n_time) {
    sample <- filtered[k, , , drop = T]
    
    if (any(is.na(sample[ref_idx, 1:3]))) {
      filtered[k, 1:3, ] <- NA
      next
    }
    
  }
  
  
  
}