# mov_correct function

library(tidyverse)
library(zoo)
library(signal)

interp_filter <- function( raw, ref_idx) {
  
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
        
        interp_tmp <- zoo::na.approx(tmp, na.rm = F)
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
  
  filtered <- array(NA_real_, dim = dim(interpolated))
  for (k in 1:dim(interpolated)[3]) {
    
    if (k %in% ref_idx) {
      filtered[ , 1:3, k] <- filtfilt(butter5, interpolated[ , 1:3, k])
    } else {
      filtered[ , 1:3, k] <- filtfilt(butter20, interpolated[ , 1:3, k])
    }
    
  }
  
  #for (i in 1:length(na_idx)) {
  #  current <- na_idx[i]
  #  samps = current[[3]]
  #  dim = current[[1]]
  #  sens = current[[2]]
  #  filtered[samps, dim, sens] <- NA
    
  #  }
  
  return(filtered)
  
}