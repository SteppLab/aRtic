#' Palate trace Function (est_palate)
#'
#' This function rotates 3D articulatory kinematic data from a palate tracing,
#' rotates and corrects the data from head movement, and then calculates an estimated palate location 
#' in the XYZ plane.
#' 
#' @param data A 3d array of the palate trace recording
#' @param coord A 3D array of the rotated coordinate plane
#' @param pl_idx The numeric value representing the palate trace sensor
#' @param base_rt The rotation matrix extracted from the rotated coordinate plane
#' @param base_center A vector with a length of 3 representing the translation vector extracted from the rotated coordinate plane
#' @return A data frame of the estimated X, Y, and Z coordinates of the palate
#' @import dplyr pracma 
#' @export
#' 

est_palate <- function(data, coord, ref_idx, pl_idx, base_rt, base_center) {
  
  n_time <- dim(data)[1]
  n_dims <- dim(data)[2]
  n_sens <- dim(data)[3]
  
  filtered_palate <- interp_filter(data, ref_idx)

  corrected_palate <- correct_mov(filtered_palate, coord, ref_idx, base_rt, base_center)
  
  palate_trace <- corrected_palate[, 1:3, ]
  
  palate <- palate_trace[ , ,pl_idx]
  palate_idx <- complete.cases(palate)
  
  mean <- colMeans(palate[palate_idx,], na.rm = T)
  
  distances <- apply(palate, 1, function(row) {
    if (any(is.na(row))) return(NA)
    sqrt(sum((row-mean)^2))
  })
  
  threshold <- mean(distances, na.rm = T) + 2*sd(distances, na.rm = T)
  
  outliers <- which(distances > threshold)
  palate_trace[outliers, , pl_idx] <- NA
  
  keep <- which(complete.cases(palate_trace[, , pl_idx]))
  
  palate_clean <- palate_trace[keep, , , drop = F]
  
  palate_coords <- palate_clean[, , pl_idx]
  
  palate_coords <- as.data.frame(palate_coords)
  
  palate_coords <- palate_coords |>
    dplyr::rename(X = V1,
                  Y = V2,
                  Z = V3)
  
  return(palate_coords)
  
}
