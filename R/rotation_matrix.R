#' Rotation Matrix Function
#'
#' Constructs a 3D rotation matrix given an axis of rotation and an angle.
#' 
#' @param axis A numeric vector of 3 representing the axis of rotation
#' @param angle A numeric value representing the angle of rotation (radians)
#' @return A 3x3 rotation matrix as a numeric matrix
#' @export
#' 

rotation_matrix <- function(axis, angle) {
  
  x <- axis[1]
  y <- axis[2]
  z <- axis[3]
  c <- cos(angle)
  s <- sin(angle)
  C <- 1 - c
  
  matrix(c(
    x*x*C + c, x*y*C - z*s, x*z*C + y*s,
    y*x*C +z*s, y*y*C + c, y*z*C - x*s,
    z*x*C - y*s, z*y*C + x*s, z*z*C +c
  ), nrow = 3, byrow = T)
  
}