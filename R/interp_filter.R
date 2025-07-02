#' Interpolation and Filtering of EMA Data
#'
#' Purpose: This function finds missing values in an EMA recording, uses linear 
#' interpolation to fill in the missing values. And then smooths the data using a 
#' low-pass 20 Hz butterworth filter.
#' 
#' @param data A string representing the name of the data matrix containing the raw EMA recording.
#' @param ref_idx A vector representing the numeric ids of the three referent sensors
#' @return The resulting 3D matrix of data
#' @import zoo signal
#' 

interp_filter <- function(raw, ref_idx) {
  
  n_time <- dim(raw)[1]
  n_dims <- dim(raw)[2]
  n_sens <- dim(raw)[3]
  
  # Interpolation
  
  na_idx <- list()
  interpolated <- array(NA, dim = dim(raw))
  
  for (i in 1:n_dims) {
    for (j in 1:n_sens){
      tmp <- raw[ ,i,j]
      k <- which(is.na(tmp))
      if (length(k) > 0) {
        
        interp_tmp <- zoo::na.approx(tmp, na.rm = F)
        interp_tmp <- zoo::na.locf(interp_tmp, na.rm = FALSE)  # forward fill
        interp_tmp <- zoo::na.locf(interp_tmp, fromLast = TRUE, na.rm = FALSE)  # backward fill
        na_idx[[length(na_idx) + 1]] <- list(i = i, j = j, k = k)
        interpolated[ ,i,j] <- interp_tmp
        
      } else {
        interpolated[ ,i,j] <- tmp
      }
    }
  }
  
  # filtering
  sr <- 100
  butter5 <- butter(3, 5 / (sr/2), type = "low")
  butter20 <- butter(3, 20 / (sr/2), type = "low")
  
  filtered <- array(NA_real_, dim = dim(interpolated))
  for (k in 1:dim(interpolated)[3]) {
    
    if (anyNA(interpolated[,,k])) {
      message(sprintf("Skipping sensor %d due to internal NAs.", k))
      next
    }
    
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