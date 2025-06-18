#' Normal Vector Calculation Function
#'
#' Calculates the normal vector in reference to a plane defined by three points
#' 
#' @param mean_data A 2D matrix representing the mean points in the X, Y, and Z dimensions of three sensors
#' @param sensor_idx A vector of the numeric ids of the three plane sensors
#' @return A vector with a length of three representing the normal vector of the plane
#' @import dplyr pracma
#' @export
#' 

norm_vec <- function(mean_data, sensor_idx) {
  
  # define points
  p1 <- mean_data[sensor_idx[1], ]
  p2 <- mean_data[sensor_idx[2], ]
  p3 <- mean_data[sensor_idx[3], ]  
  
  # Defining two vectors from plane points
  
  V1 <- p1 - p3   
  V2 <- p2 - p3
  
  # Calculating the normal vector
  
  norm_vec <- pracma::cross(V2, V1)
  norm_vec <- norm_vec / sqrt(sum(norm_vec^2))
  
  return(norm_vec)
  
  
}