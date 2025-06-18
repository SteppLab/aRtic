#' Define Coordinate Plane Function
#'
#' Purpose: This function rotates 3D articulatory kinematic data from a bite plan recording
#' to redefine a coordinate plane for movement data.
#' 
#' @param data A string representing the name of the data matrix (imported from load_tsv)
#' @param ref_idx A vector of the numeric ids of the three referent sensors
#' @param bp_idx A vector of the numeric ids of the bite plane sensors
#' @return A 3D array of the rotated coordinate data for the referent and bp sensors
#' @import dplyr pracma
#' @export
#' 

define_coord <- function(data, ref_idx, bp_idx) {
  
  # Checking is the minimum inputs are there
  if (missing(data) || missing(ref_idx)){
    stop('Usage: define_coord(data,ref_idx, bp_idx')
  }
  
  # Checking to see if there are at least three referent sensors
  n_refs <- length(ref_idx)
  if (n_refs < 3) {
    stop('Need three referent sensors')
  }
  
  dims <- dim(data)
  n_time <-dim(data)[1] # Number of Time Points (samples)
  n_dims <-dim(data)[2] # Number of dimensions 
  n_sens <-dim(data)[3] # Number of Sensors

  # Mean location of palate and referent points
  subset_data <- data[, 1:3, ]
  
  mean_data <- apply(subset_data, c(2, 3), function(x) mean(x, na.rm = T)) |>
    t()
  
  # Computing the normal vector of the bite plane
  normal_vec <- norm_vec(mean_data, bp_idx)
  
  # Setting the referent vector
  ref_vec <- c(0, -1, 0)
  
  # Computing rotation axis and angle between the normal vector and referent vector
  axis <- pracma::cross(normal_vec, ref_vec)
  axis <- axis/sqrt(sum(axis^2))
  angle <- acos(pracma::dot(normal_vec, ref_vec))
  
  base <- rotation_matrix(axis, angle)
  
  rot_center <- center(mean_data, bp_idx)
  
  # Rotate data
  rotated_data <- array(NA, dim = c(dim(data)[1], 3, dim(data)[3]))
  
  for (t in 1:dim(data)[1]) {
    for (s in 1:dim(data)[3]) {
      vec <- data[t, 1:3, s]
      rotated_data[t, , s] <- base %*% (vec - rot_center)
    }
  }
  
  # Defining normal vector and center of the head plane
  head_vec <- norm_vec(mean_data, ref_idx)
  
  # Computing rotation axis and angle between normal vector and head vector
  head_axis <- pracma::cross(normal_vec, head_vec)
  head_axis <- head_axis/sqrt(sum(head_axis^2))
  head_angle <- acos(pracma::dot(normal_vec, head_vec))
  
  head_rt <- rotation_matrix(head_axis, head_angle)
  
  head_center <- center(mean_data, ref_idx)
  
  return(list(
    rotated_data = rotated_data,
    base_rt = base,
    base_center = rot_center,
    head_rt = head_rt,
    head_center = head_center))

}







