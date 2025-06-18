#' Correct Movement Function (correct_mov)
#'
#' Purpose: This function rotates 3D articulatory kinematic data from a data recording
#' in reference to rotated bite plane data.
#' 
#' @param filtered A 3d array of interpolated and filtered data from the data recording
#' @param ref_idx A vector of the numeric ids of the three referent sensors
#' @param rotation A rotation matrix extracted from define_coord
#' @param center A vector with a length of 3 representing the translation vector extracted from define_coord
#' @return A 3D array of the rotated data from the data recording
#' @import dplyr pracma
#' @export
#' 
correct_mov <- function(filtered, ref_idx, base_rt, base_center, ref_rt, ref_center) {
  
  n_time <- dim(filtered)[1]
  n_sens <- dim(filtered)[3]
  
  aligned <- array(NA_real_, dim = c(n_time, 3, n_sens))
  
  for (t in 1:n_time) {
    for(s in 1:n_sens) {
      vec <- filtered[t, 1:3, s]
      aligned[t, ,s] <- as.vector(base_rt %*% (vec - base_center))
    }
  }
  
  #mean_data <- apply(aligned[, , ref_idx], c(2,3), mean, na.rm = T)
  
  #head_vec <- norm_vec(mean_data, ref_idx)
  
  #axis <- pracma::cross(head_vec, ref_vec)
  #axis <- axis/sqrt(sum(axis^2))
  #angle <- acos(pracma::dot(head_vec, ref_vec))
  
  #center <- center(mean_data, ref_idx)
  
  #head_rt <- rotation_matrix(axis, angle)
  
  n_time <- dim(aligned)[1]
  n_sens <- dim(aligned)[3]
  
  corrected <- array(NA_real_, dim = dim(aligned))
  
  for (k in 1:n_time) {
    for (r in 1:n_sens) {
      vec <- aligned[k, 1:3, r]
      corrected[k, , r] <- as.vector(ref_rt %*% (vec - ref_center))
    }
  }
  
  return(aligned)
  
}

