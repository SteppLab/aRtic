#' Palate trace Function (est_palate)
#'
#' Purpose: This function rotates 3D articulatory kinematic data from a palate tracing,
#' rotates and corrects the data from head movement, and then calculates an estimated palate location 
#' in the XYZ plane.
#' 
#' @param data A 3d array of the palate trace recording
#' @param ref_idx A vector of the numeric ids of the three referent sensors
#' @param rotation A rotation matrix extracted from define_coord
#' @param center A vector with a length of 3 representing the translation vector extracted from define_coord
#' @return A 3D array of the rotated data from the data recording
#' @import dplyr pracma correct_mov interp_filter
#' @export
#' 

est_palate <- function(data, ref_idx, pl_idx rotation, center) {
  
  n_time <- dim(data)[1]
  n_dims <- dim(data)[2]
  n_sens <- dim(data)[3]
  
  filtered_palate <- interp_filter(data, ref_idx)

  corrected_palate <- correct_mov(filtered_palate, ref_idx, rotation, center)
  
  
  
}