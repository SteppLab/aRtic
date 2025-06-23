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
#' @return A 2D matrix of the estimated X, Y, and Z coordinates of the palate
#' @import dplyr pracma geometry sp correct_mov interp_filter
#' @export
#' 

est_palate <- function(data, coord, ref_idx, pl_idx, base_rt, base_center) {
  
  n_time <- dim(data)[1]
  n_dims <- dim(data)[2]
  n_sens <- dim(data)[3]
  
  filtered_palate <- interp_filter(data, ref_idx)

  corrected_palate <- correct_mov(filtered_palate, coord, ref_idx, base_rt, base_center)
  
  palate_trace <- corrected_palate[, 1:3, ]
  
  ref_mean <- apply(palate_trace[, , ref_idx], c(2, 3), mean, na.rm = T) |>
    t()
  
  normal_vec <- norm_vec(ref_mean, ref_idx)
  
  s1 <- ref_mean[1, ]
  s2 <- ref_mean[2, ]
  s3 <- ref_mean[3, ]
  
  v1 <- s2 - s1
  
  u <- v1/sqrt(sum(v1^2))
  v <- pracma::cross(normal_vec, u)
  
  origin <- s1
  triangle_2d <- rbind(
    to_2d(s1, origin, u, v),
    to_2d(s2, origin, u, v),
    to_2d(s3, origin, u, v)
  )
  
  in_triangle <- logical(n_time)
  
  for (t in 1:n_time) {
    
    pt <- palate_trace[t, ,pl_idx]
    pt_proj <- project_point_to_plane(pt, origin, normal_vec)
    pt_2d <- to_2d(pt_proj, origin, u, v)
    
    in_triangle <- sp::point.in.polygon(
      pt_2d[1],
      pt_2d[2],
      triangle_2d[, 1],
      triangle_2d[, 2]
      ) > 0 
      
    if (!in_triangle) {
      palate_trace[t, , pl_idx] <- NA
    }
    
  }
  
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
  
  coords <- scale(palate_coords, center = T, scale = F)
  
  pca <- prcomp(coords)
  
  pc1_vals <- pca$x[ , 1]
  
  spline_x <- smooth.spline(x = pc1_vals, y = coords[, 1], spar = .8)
  spline_y <- smooth.spline(x = pc1_vals, y = coords[, 2], spar = .8)
  spline_z <- smooth.spline(x = pc1_vals, y = coords[, 3], spar = .8)
  
  s_grid <- seq(min(pc1_vals), max(pc1_vals), length.out = 200)
  
  fit_x <- predict(spline_x, s_grid)$y
  fit_y <- predict(spline_y, s_grid)$y
  fit_z <- predict(spline_z, s_grid)$y
  
  spline_df <- data.frame(X = fit_x, Y = fit_y, Z = fit_z)
  
  return(spline_df)

}

project_point_to_plane<- function(x, origin, normal) {
  x_proj <- x - sum((x - origin) * normal) * normal
  return(x_proj)
}


to_2d <- function(x_proj, origin, u, v) {
  vec <- x_proj - origin
  c(sum(vec * u), sum(vec * v))
}
