#' Correct Movement Function (correct_mov)
#'
#' This function rotates 3D articulatory kinematic data from a recording
#' in reference to rotated bite plane data.
#' 
#' @param filtered A 3d array of interpolated and filtered data from the data recording
#' @param coord A 3d array of the rotated bite plane data
#' @param ref_idx A vector of the numeric ids of the three referent sensors
#' @param base_rt A rotation matrix extracted from define_coord
#' @param base_center A vector with a length of 3 representing the translation vector extracted from define_coord
#' @return A 3D array of the rotated data from the data recording
#' @import dplyr pracma
#' 

correct_mov <- function(filtered, coord, ref_idx, base_rt, base_center) {
  
  n_time <- dim(filtered)[1]
  n_sens <- dim(filtered)[3]
  
  aligned <- array(NA_real_, dim = c(n_time, 3, n_sens))
  
  for (t in 1:n_time) {
    for(s in 1:n_sens) {
      vec <- filtered[t, 1:3, s]
      aligned[t, ,s] <- as.vector(base_rt %*% (vec - base_center))
    }
  }
  
  ref_target <- apply(coord[, , ref_idx], c(2,3), mean, na.rm = T)
  corrected <- array(NA, dim = dim(aligned))
  
  for (k in 1:n_time) {
    frame <- aligned[k, , ,drop = T]
    refs <- frame[1:3, ref_idx, drop = F]
    
    if (any(is.na(refs))) {
      corrected[k, , ] <- NA
      next
    }
    
    refs_t <- t(refs)
    target_t <- t(ref_target)
    
    mean_refs <- colMeans(refs_t)
    mean_target <- colMeans(target_t)
    
    refs_centered <- sweep(refs_t, 2, mean_refs)
    target_centered <- sweep(target_t, 2, mean_target)
    
    result <- pracma::kabsch(refs_centered, target_centered)
    R <- result$U 
    
    frame_t <- t(frame)
    frame_centered <- sweep(frame_t, 2, mean_refs)
    
    rotated <- (R %*% t(frame_centered))
    rotated <- t(rotated)
    
    aligned_frame <- sweep(rotated, 2, -mean_target, FUN = "-") * -1
    aligned_frame <- sweep(rotated, 2, mean_target, FUN = "+")
    
    corrected[k, , ] <- t(aligned_frame)
  
  }
  
  return(corrected)
  
}
