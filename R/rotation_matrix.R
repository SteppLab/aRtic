

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