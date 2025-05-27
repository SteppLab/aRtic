# Correct Movement Function

library(tidyverse)


correct_mov <- function(coord, filtered, ref_idx, bp_idx) {
  
  n_time <- dim(filtered)[1]
  n_dims <- dim(filtered)[2]
  n_sens <- dim(filtered)[3]
  
  coord_av <- apply(coord, c(2, 3), mean, na.rm = T)
  corrected <- array(NA, dim = dim(filtered))  # Initialize output
  
  for (k in 1:n_time) {
    sample <- filtered[k, , , drop = T]
    
    if (any(is.na(sample[1:3, ref_idx]))) {
      filtered[k, 1:3, ] <- NA
      next
    }
    
    RT <- .compute_rt(sample[1:3, ref_idx], coord_av[1:3, ref_idx])
    pos <- cbind(t(sample[1:3, 1:n_sens]), matrix(1, nrow = n_sens, ncol = 1)) %*% RT
    corrected[k, 1:3, ] <- t(pos[ , 1:3])
    
  }
  
  return(corrected)
  
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
  q <- Re(q)
  q <- q/sqrt(sum(q^2))
  q0 <- q[1]
  qx <- q[2]
  qy <- q[3]
  qz <- q[4]
  
  R <- matrix(c(
    1 - 2*(qy^2 + qz^2),   2*(qx*qy - qz*q0),     2*(qx*qz + qy*q0),
    2*(qx*qy + qz*q0),     1 - 2*(qx^2 + qz^2),   2*(qy*qz - qx*q0),
    2*(qx*qz - qy*q0),     2*(qy*qz + qx*q0),     1 - 2*(qx^2 + qy^2)
  ), nrow = 3, byrow = TRUE)
  
  roll <- atan2(R[2,3], R[3,3])
  pitch <- -asin(R[1,3])
  yaw <- atan2(R[1,2], R[1,1])
  
  mean1 <- colMeans(M1, na.rm = TRUE)
  mean2 <- colMeans(M2, na.rm = TRUE)
  t <- mean2 - R %*% mean1
  t <- as.vector(t)
  
  RT <- diag(4)
  RT[1:3, 1:3] <- R
  RT[1:3, 4] <- t
  RT[4, 1:3] <- 0
  RT[4, 4] <- 1
  
  return(RT)


}

.detrend <- function(mat) {
  
  dims <- dim(mat)
  detrended <- array(NA_real_, dim = dims)
  
  if (length(dims) == 2) {
    
    for (i in seq_len(dims[2])) {
      
      y <- mat[, i]
      if (all(is.na(y))) next
      
      detrended[ ,i] <- y - mean(y, na.rm = T)
      
    }
    
  } else if (length(dims) == 3) {
    
    for (i in seq_len(dims[2])) {
      for (j in seq_len(dims[3])) {
        y <- mat[, i, j]
        if (all(is.na(y))) next
        detrended[ , i, j] <- y - mean(y, na.rm = T)
      }
    }
  } else {
    
    stop("Input must be a 2D or 3D array.")
    
  }
  
  return(detrended)
  
}
