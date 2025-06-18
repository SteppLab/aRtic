#' Translation Vector Function
#'
#' Calculates the translation vector for rotation
#' 
#' @param mean_data A 2D matrix representing the mean points in the X, Y, and Z dimensions of three sensors
#' @param sensor_idx A vector of the numeric ids of the three plane sensors
#' @return A vector with a length of three representing the tranlation vector for rotation
#' @import dplyr pracma
#' @export
#' 

center <- function(mean_data, sensor_idx) {
  
  # define points
  p1 <- mean_data[sensor_idx[1], ]
  p2 <- mean_data[sensor_idx[2], ]
  p3 <- mean_data[sensor_idx[3], ]  
  
  center <- (p1 + p2 + p3)/3
  
  return(center)
  
}