#'Cross product function
#'
#' Purpose: This function computes the 3D cross product between two vectors.
#' 
#' @param a A vector represting the position between two points in space
#' @param b A vector represting the position between two points in space
#' @return A vector that is perpendicular to both inputs
#' @export
#'


cross_product <- function(a, b) {
  c( a[2]*b[3] - a[3]*b[2],
     a[3]*b[1] - a[1]*b[3],
     a[1]*b[2] - a[2]*b[1]
  )
}