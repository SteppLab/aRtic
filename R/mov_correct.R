# mov_correct function

library(tidyverse)
library(zoo)
library(signal)

mov_correct <- function(coord, raw, ref_idx) {
  
  raw_data <- raw[[1]]
  n_time <- dim(raw_data)[1]
  n_dims <- dim(raw_data)[2]
  n_sens <- dim(raw_data)[3]
  
  # Interpolation
  
  na_idx <- list()
  interpolated <- array(NA, dim = dim(raw_data))
  
  for (i in 1:n_dims) {
    for (j in 1:n_sens){
      tmp <- raw_data[ ,i,j]
      k <- which(ios.na(tmp))
      if (length(k) > 0) {
        
        interp_tmp <- z00::na.approx(tmp, na.rm = F)
        na_idx[[length(na_idx) + 1]] <- list(i = j, j = j, k = k)
        interpolated <- [ ,i,j] <- tmp
        
      } else {
        interpolated[ ,i,j] <- tmp
      }
    }
  }
  
  # filtering
  sr <- 100
  butter5 <- butter(3, 5 / (sr/2), type ="low")
  butter20 <- butter(3, 20 / (sr/2), type ="low")
  
  
  
  
}