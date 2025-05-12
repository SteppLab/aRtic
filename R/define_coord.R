#' Define Coordinate Plane Function
#'
#' Purpose: This function rotates 3D articulatory kinematic data from a bite plan recording
#' to redefine a coordinate plane for movement data.
#' 
#' @param data A string representing the name of the data matrix (imported from load_tsv)
#' @param ref_idx A vector of the numeric ids of the three referent sensors
#' @param bp_idx A vector of the numeric ids of the bite plane sensors
#' @return A 3D array of the rotated coordinate data for the referent and bp sensors
#' @import dplyr readr abind pracma
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
  
  data_3D <- data[[1]]
  
  dims <- dim(data_3D)
  n_time <-dim(data_3D)[1] # Number of Time Points (samples)
  n_dims <-dim(data_3D)[2] # Number of dimensions 
  n_sens <-dim(data_3D)[3] # Number of Sensors


  # Mean location of palate and referent points
  all_idx <- c(ref_idx, bp_idx)
  
  subset_data <- data_3D[, 1:3, all_idx]
  
  mean_data <- apply(subset_data, c(2, 3), function(x) mean(x, na.rm = T)) |>
    t()
  
  # define vector of angles
  p5 <- mean_data[4, ]  # Sensor 5
  p6 <- mean_data[5, ]  # Sensor 6
  p7 <- mean_data[6, ]  # Sensor 7
  
  # Normal vector of the palate plane (5, 6, 7) divide each element by size of vector
  
  V1 <- p5 - p7 # bp point 5 - bp point 7  
  V2 <- p6 - p7 # bp point 6 - bp point 7
  
  center <- (p5 + p6 + p7)/ 3
  
  norm_vec <- pracma::cross(V2, V1)
  norm_vec <- -norm_vec
  norm_vec <- norm_vec / sqrt(sum(norm_vec^2))
  
  # Setting the referent vector
  ref_vec <- c(0, 0, 1)
  
  # Computing rotation axis and angle between the norm vector and referent vector
  axis <- pracma::cross(norm_vec, ref_vec)
  axis <- axis/sqrt(sum(axis^2))
  angle <- acos(pracma::dot(norm_vec, ref_vec))
  
  R <- rotation_matrix(axis, angle)
  
  pitch <- asin(-R[3,1])
  roll <- atan2(R[3,2], R[3,3])
  yaw <- atan2(R[2,1], R[1,1])
  
  angles <- c(roll, pitch, yaw)
  
  
  base <- euler(angles)
  
  # Rotate data (example for rotating each point in data_3D)
  rotated_data <- array(NA, dim = c(dim(data_3D)[1], 3, dim(data_3D)[3]))
  
  for (t in 1:dim(data_3D)[1]) {
    for (s in 1:dim(data_3D)[3]) {
      vec <- data_3D[t, 1:3, s]
      rotated_data[t, , s] <- base %*% (vec - center) + center
    }
  }
  
  return(rotated_data)

}







