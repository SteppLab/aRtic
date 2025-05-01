# define_coord Function

# Purpose: compute reference coordinate plane for 3D articulatory kinematic data

library(tidyverse)
library(abind)

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

make_RT <- function(par) {
  RT <- diag(4)
  RT[1:3, 1:3] <- euler(par[1:3])  
  RT[4, 1:3] <- par[4:6]
  RT
}

objective <- function(par, data_subset, constraint) {
  RT <- make_RT(par)
  homog <- cbind(data_subset, 1)
  fit <- homog |>
    RT
  valid <- !is.na(constraint)
  sum((fit[valid] - constraint[valid])^2)
  
}

define_coord <- function(data, ref_idx, bp_idx = NULL, constraint = NULL) {
  
  # Checking is the minimum inputs are there
  if (missing(data) || missing(ref_idx)){
    stop('Usage: define_coord(data,ref_idx, bp_idx = NULL, constraint = NULL')
  }
  
  # Checking to see if there are at least two referent sensors
  n_refs <- length(ref_idx)
  if (n_refs < 2) {
    stop('Need at least two referent sensors')
  }
  
  n_time <-dim(data)[1] # Number of Time Points (samples)
  n_dims <-dim(data)[2] # Number of dimensions 
  n_sens <-dim(data)[3] # Number of Sensors
  
  # Case when there are only two referent sensors
  if (n_refs == 2) {
    angles <- data[, 4:6, ref_idx] * pi / 180
    
    # Define pairings of referent sensors
    idx <- matrix(c(1,2,1,3,2,3), ncol = 2)
    
    # Initializing df for coordinates
    xyz_list <- list()
    
    for (k in 1:nrow(idx)) {
      
      x <- sin(a[, idx[k, 1], ]) * 10
      y <- sin(a[, idx[k, 2], ]) * 10
      z <- cos(a[, idx[k, 1], ]) * 10
      
      # Combine into a new xyz matrix, filling with NAs for unused dimensions
      xyz <- array(NA, dim = c(length(x), n_dims, 2))
      xyz[, 1:3, ] <- cbind(x, y, z) + data[, 1:3, ref_idx]  # Add offset from reference
      xyz_list[[k]] <- xyz
      
    }
    
    # Combine data with original data
    data <- abind::abind(data, do.call(cbind, xyz_list, along = 3))
    
  }
  
  return(data)

  # if no constraint present, calculating mean and reorder sensors
  if (is.null(bp_idx) || length(bp_idx == 0)) {
    
    basis <- apply(data[, 1:3, ref_idx, drop = F], c(2,3), function(x) mean(x, na.rm = T))
    basis <- t(basis)
    return(basis)
    
  }
  
  if (is.null(constraint)) {
    constraint <- rbind(matrix(NA, nrow = n_refs, ncol = 3),
                        matrix(c(0, NA, 0,
                                 NA, 0, 0,
                                 0, NA, 0), byrow = T, ncol = 3))
  } else {
    constraint <- rbind(matrix(NA, nrow = n_refs, ncol = 3), constraint)
  }
  
  
  
  
  
  
  # Mean positions of ref and bp_idx
  data_subset <- data[, 1:3, c(refIdx, bpIdx)] |>
    apply(c(2, 3), mean, na.rm = T) |>
    t()
  
  
  
  
}
