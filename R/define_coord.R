#' Define Coordinate Plane Function
#'
#' Purpose: This function rotates 3D articulatory kinematic data from a bite plan recording
#' to redefine a coordinate plane for movement data.
#' 
#' @param data A string representing the name of the data matrix (imported from load_tsv)
#' @param ref_idx A vector of the ids of the three referent sensors
#' @param bp_idx A vecotr of the number ids of the bite plane sensors
#' @return A 3D array of the rotated coordinate data for the referent and bp sensors
#' @import dplyr readr abind
#' @export
#' 
library(tidyverse)
library(abind)

.euler <- function(angles) {
  x <- angles[1]
  y <- angles[2]
  z <- angles[3]
  
  Rx <- matrix(c(1, 0, 0,
                 0, cos(x), -sin(x),
                 0, sin(x), cos(x)), nrow = 3, byrow = T)
  
  Ry <- matrix(c(cos(y), 0, sin(y),
                 0, 1, 0,
                 -sin(y), 0, cos(y)), nrow = 3, byrow = T)
  
  Rz <- matrix(c(cos(z), -sin(z), 0,
                 sin(z), cos(z), 0,
                 0, 0, 1), nrow = 3, byrow = T)
  
  R <- Rx %*% Ry %*% Rz
  return(R)
}

.cross_product <- function(a, b) {
  c( a[2]*b[3] - a[3]*b[2],
     a[3]*b[1] - a[1]*b[3],
     a[1]*b[2] - a[2]*b[1]
     )
}


define_coord <- function(data, ref_idx, bp_idx) {
  
  # Checking is the minimum inputs are there
  if (missing(data) || missing(ref_idx)){
    stop('Usage: define_coord(data,ref_idx, bp_idx = NULL, constraint = NULL')
  }
  
  # Checking to see if there are at least three referent sensors
  n_refs <- length(ref_idx)
  if (n_refs < 3) {
    stop('Need at least three referent sensors')
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
  
  # Normal vector of the palate plane (5, 6, 7) divide each element by size of vector
  
  V1 <- mean_data[5, ] - mean_data[4, ] # ref point 2 - ref point 1
  V2 <- mean_data[6, ] - mean_data[4, ] # bp point - ref point 1
  
  norm_vec <- .cross_product(V1, V2)
  norm_vec <- norm_vec / sqrt(sum(norm_vec^2))
  
  # normal vector of palate to calculate theta and phi angles
  
  x <- norm_vec[1]
  y <- norm_vec[2]
  z <- norm_vec[3]
  
  # Inclination (theta): angle from z-axis, range [0, π]
  theta <- acos(z) * 180/pi
  
  # Azimuth (phi): angle from x-axis in xy-plane, range [-π, π]
  phi <- atan2(y, x) * 180/pi
  
  # Calculating roll
  
  # define vector of angles
  p1 <- mean_data[1, ]  # Sensor 1
  p2 <- mean_data[2, ]  # Sensor 2
  p7 <- mean_data[6, ]  # Sensor 7
  
  # Create the vector along the line between sensor 1 and sensor 2
  line_1_to_2 <- p2 - p1
  
  # Create the vector from sensor 1 to sensor 7
  line_1_to_7 <- p7 - p1
  
  # Compute the normal vector perpendicular to both line_1_to_2 and line_1_to_7
  roll_vec <- .cross_product(line_1_to_2, line_1_to_7)
  
  # Normalize the normal vector
  roll_vec <- roll_vec / sqrt(sum(roll_vec^2))
  
  # Compute the dot product between the normal vector and line_1_to_2
  dot_product_line <- sum(roll_vec * line_1_to_2)
  
  # Compute the cross product between the normal vector and line_1_to_2
  cross_product_line <- .cross_product(roll_vec, line_1_to_2)
  
  # Compute the roll angle using the line between sensors 1 and 2 as the reference
  roll <- atan2(sqrt(sum(cross_product_line^2)), dot_product_line) * 180 / pi
  
  # euler rotation matrix to rotate data 
  
  angles <- c(roll, theta, phi)
  
  base <- .euler(angles)
  
  # Rotate data (example for rotating each point in data_3D)
  rotated_data <- array(NA, dim = c(dim(data_3D)[1], 3, dim(data_3D)[3]))
  
  for (t in 1:dim(data_3D)[1]) {
    for (s in 1:dim(data_3D)[3]) {
      vec <- data_3D[t, 1:3, s]
      rotated_data[t, , s] <- base %*% vec
    }
  }
  
  return(rotated_data)

}







