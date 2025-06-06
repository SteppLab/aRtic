#' Palate trace Function (est_palate)
#'
#' Purpose: This function rotates 3D articulatory kinematic data from a palate tracing,
#' rotates and corrects the data from head movement, and then calculates an estimated palate location 
#' in the XYZ plane.
#' 
#' @param data A 3d array of the palate trace recording
#' @param ref_idx A vector of the numeric ids of the three referent sensors
#' @param rotation A rotation matrix extracted from define_coord
#' @param center A vector with a length of 3 representing the translation vector extracted from define_coord
#' @return A 3D array of the rotated data from the data recording
#' @import dplyr pracma correct_mov interp_filter
#' @export
#' 

est_palate <- function(data, ref_idx, pl_idx, rotation, center) {
  
  n_time <- dim(data)[1]
  n_dims <- dim(data)[2]
  n_sens <- dim(data)[3]
  
  filtered_palate <- interp_filter(data, ref_idx)

  corrected_palate <- correct_mov(filtered_palate, ref_idx, rotation, center)
  
  palate_trace <- corrected_palate[, 1:3, ]
  
  ref_mean <- apply(palate_trace[, , ref_idx], c(2, 3), mean, na.rm = T)
  s1 <- ref_mean[,1]
  s2 <- ref_mean[,2]
  s3 <- ref_mean[,3]
  
  s1_s2 <- s2 - s1
  s1_s3 <- s3 - s1
  norm_vec <- pracma::cross(s1_s2, s1_s3)
  norm_vec <- norm_vec/sqrt(sum(norm_vec^2))
  
  u <- s1_s2 / sqrt(sum(s1_s2^2))
  v <- pracma::cross(norm_vec, u)
  
  to_2d <- function(P) {
    rel <- P - s1
    c(sum(rel * u), sum(rel * v))
  }
  
  tri_2d <- t(sapply(list(s1, s2, s3), to_2d))
  
  stylus <- data[ , , 8]  # stylus data
  inside <- logical(nrow(stylus))
  
  for (t in 1:nrow(stylus)) {
    pt <- stylus[t, ]
    if (any(is.na(pt))) next
    
    pt_2d <- to_2d(pt)
    
    
  }
  
  stylus_filtered <- stylus
  stylus_filtered[!inside, ] <- NA
  
  data_filtered <- data
  data_filtered[ , , 8] <- stylus_filtered
  
}