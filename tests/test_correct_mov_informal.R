library(tidyverse)
library(readr)
library(plotly)
library(abind)
library(here)
library(pracma)
library(signal)
library(zoo)
source(".\\R\\load_tsv.R")
source(".\\R\\define_coord.R")
source(".\\R\\rotation_matrix.R")
source(".\\R\\interp_filter.R")
source(".\\R\\correct_mov.R")
source(".\\R\\norm_vec.R")
source(".\\R\\center.R")

bite_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv"))

ref_idx <- c(1,2,3)
bp_idx <- c(5,6,7)

bite_data_3d <- bite_data[[1]]

rotated <- define_coord(bite_data_3d, ref_idx, bp_idx)

rotated_plane <- rotated[[1]]

base_rt <- rotated[[2]]

base_center <- rotated[[3]]

ref_rt <- rotated[[4]]

ref_center <- rotated[[5]]

sensor_data <- load_tsv(here("tests", "sample_data", "PLURAL02_RP.tsv"))

sensor_data_3d <- sensor_data[[1]]

filtered <- interp_filter(sensor_data_3d, ref_idx)

corrected <- correct_mov(filtered, ref_idx, base_rt, base_center, ref_rt, ref_center)

n_time <- dim(corrected)[1]

trajectory <- lapply(seq_along(ref_idx), function(i) {
  s <- ref_idx[i]
  df <- as.data.frame(corrected[, 1:3, s])
  colnames(df) <- c("X", "Y", "Z")
  df$Time <- 1:n_time
  df$Sensor <- paste0("Ref", s)
  return(df)
})

traj_df <- do.call(rbind, trajectory)

plot_ly(traj_df,  x = ~X, y = ~Y, z = ~Z, color = ~Sensor, type = "scatter3d",
        mode = "lines+markers") |>
  layout(scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z")),
         title = "Reference Sensor Trajectories (After Correction)")


orig <- lapply(seq_along(ref_idx), function(i) {
  s <- ref_idx[i]
  df <- as.data.frame(interpolated[, 1:3, s])
  colnames(df) <- c("X", "Y", "Z")
  df$Time <- 1:n_time
  df$Sensor <- paste0("Ref", s)
  return(df)
})

orig_df <- do.call(rbind, orig)

plot_ly(orig_df,  x = ~X, y = ~Y, z = ~Z, color = ~Sensor, type = "scatter3d",
        mode = "lines+markers") |>
  layout(scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z")),
         title = "Reference Sensor Trajectories (Before Correction)")

