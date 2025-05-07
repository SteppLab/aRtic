#' Euler Angle Function
#'
#' Purpose: This function calculates the euler matrix to rotate 3D articulatory data.
#' 
#' @param angles A vector containing the rotation angles roll, phi, and theta
#' @return A rotation matrix
#' @import dplyr readr abind
#' @export
#'

euler <- function(angles) {
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