#' Normal Vector Calculation Function
#'
#' Calculates the normal vector in reference to a plane defined by three points
#' 
#' @param data A string representing the name of the data matrix (imported from load_tsv)
#' @param ref_idx A vector of the numeric ids of the three referent sensors
#' @param bp_idx A vector of the numeric ids of the bite plane sensors
#' @return A list containing the normal vector of the plane and the center point for
#' rotation and translation
#' @import dplyr pracma
#' @export
#' 
#' 

norm_vec <- function(mean_data, sensor_idx, center = c("mean", "point"), center_pt = NULL) {
  
  # define points
  p1 <- mean_data[sensor_idx[1], ]
  p2 <- mean_data[sensor_idx[2], ]
  p3 <- mean_data[sensor_idx[3], ]  
  
  # Defining two vectors from plane points
  
  V1 <- p1 - p3   
  V2 <- p2 - p3
  
  if (center == "mean") {
    
    center = (p1 + p2 + p3) / 3
    
  } else {
    
    center <- center_pt
    
  }
  
  # Calculating the normal vector
  
  norm_vec <- pracma::cross(V2, V1)
  norm_vec <- norm_vec / sqrt(sum(norm_vec^2))
  
  return(norm_vec)
  
  
}