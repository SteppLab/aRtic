# Correct Movement Function

library(tidyverse)


correct_mov <- function(coord, filtered, ref_idx, bp_idx) {
  
  n_time <- dim(filtered)[1]
  n_dims <- dim(filtered)[2]
  n_sens <- dim(filtered)[3]
  
  for (k in 1:n_time) {
    sample <- filtered[k, , , drop = T]
    
    if (any(is.na(sample[ref_idx, 1:3]))) {
      filtered[k, 1:3, ] <- NA
      next
    }
    
    RT <- .compute_rt(sample[ref_idx, 1:3], coord)
    pos <- cbind(sample[1:n_sens, 1:3], rep(1, n_sens,1)) %*% RT
    data[k, 1:3, ] <- t(pos[ , 1:3])
    
    
    
  }
  
  
  
}

.compute_rt <- function (M1, M2) {
  
  dM1 <- .detrend(M1)
  dM2 <- .detrend(M2)
  
  M <- t(dM1) %*% dM2
  
 
  Sxx <- M[1,1]; Sxy <- M[1,2]; Sxz <- M[1,3]
  Syx <- M[2,1]; Syy <- M[2,2]; Syz <- M[2,3]
  Szx <- M[3,1]; Szy <- M[3,2]; Szz <- M[3,3]
  
  N <- matrix(c(
    Sxx + Syy + Szz, Syz - Szy, Szx -Sxz, Sxy - Syx,
    Syz - Szy, Sxx - Syy - Szz, Sxy + Syx, Szx + Sxz,
    Szx - Sxz, Sxy + Syx, -Sxx + Syy - Szz, Syz + Szy,
    Sxy - Syz, Szx + Sxz, Syz + Szy, -Sxx - Syy + Szz),
    nrow = 4, byrow = T)
  
  eig <- eigen(N)
  values <- eig$values
  vectors <- eig$vectors
  
  q <- vectors[, which.max(values)]
  q0 <- q[1]
  qx <- q[2]
  qy <- q[3]
  qz <- q[4]
  
  R <- matrix(c(q0^2 + qx^2 - qy^2, 2*(qx*qy -q0*qz), 2*(qx*qz +q0*qy),
                2*(qy*qx + q0*qz), q0^2 - qx^2 + qy^2 - qz ^2, 2*(qy*qz - q0*qx),
                2*(qz*qx - q0*qy), 2*(qz*qy + q0*qx), q0^2 - qx^2 - qy^2 +qz^2),
              nrow = 3, byrow = T)
  
  roll <- atan2(R[2,3], R[3,3])
  pitch <- -asin(R[1,3])
  yaw <- atan2(R[1,2], R[1,1])
  
  t <-  colMeans(M2, na.rm = T) - (colMeans(M1, na.rm = T) %*% R)
  
  RT <- diag(4)
  RT[1:3, 1:3] <- R
  RT[4, 1:3] <- t
  
  xfm <- c(roll, pitch, yaw, t)

  
  return(xfm)


}

.detrend <- function(mat, method = "constant") {
  
  detrended <- array(NA_real_, dim = dim(mat))
  
  for (i in seq_length(dim(mat)[2])) {
    for (j in seq_length(dim(mat[3]))) {
      y <- mat[, i, j]
      if (all(is.na(y))) next
      
      detrended[ , i, j] <- y - mean(y, na.rm = T)
      
    }
  }
  
  return(detrended)
  
}