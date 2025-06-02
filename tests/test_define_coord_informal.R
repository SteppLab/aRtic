library(tidyverse)
library(readr)
library(plotly)
library(abind)
library(here)
library(pracma)
source(".\\R\\load_tsv.R")
source(".\\R\\define_coord.R")
source(".\\R\\rotation_matrix.R")

data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv"))

ref_idx <- c(1,2,3)
bp_idx <- c(5,6,7)

data <- data[[1]]

rotated <- define_coord(data, ref_idx, bp_idx)

n_dims <- dim(data)[1]

original <- data[, 1:3, ]

original_df <- list()

for (sensor in 1:8) {
  
  sensor_data <- original[, 1:3, sensor]
  
  axis_name <- c("X", "Y", "Z")
  
  for (axis in 1:3) {
    df <- data.frame(
      sensor_id = rep(sensor, n_dims),
      axis = rep(axis_name[axis], n_dims),
      n_time <- 1:n_dims,
      value = sensor_data[, axis]
    )
    
    original_df[[length(original_df) + 1]] <- df
    
  }
  
}

df_orig <- do.call(rbind,original_df)

df_orig_wide <- df_orig |>
  pivot_wider(names_from = "axis",
              values_from = "value")

plot_original <- plot_ly(df_orig_wide, x = ~X, y = ~Y, z = ~Z, color = ~factor(sensor_id), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = 'X'),
    yaxis = list(title = 'Y'),
    zaxis = list(title = 'Z')
  ))

plot_original

rotated_df <- list()

n_dims <- dim(rotated)[1]

for (sensor in 1:8) {
  
  sensor_data <- rotated[, 1:3, sensor]
  
  axis_name <- c("X", "Y", "Z")
  
  for (axis in 1:3) {
    df <- data.frame(
      sensor_id = rep(sensor, n_dims),
      axis = rep(axis_name[axis], n_dims),
      n_time <- 1:n_dims,
      value = sensor_data[, axis]
    )
    
    rotated_df[[length(rotated_df) + 1]] <- df
    
  }
  
}

df_rot <- do.call(rbind,rotated_df)

df_rot_wide <- df_rot |>
  pivot_wider(names_from = "axis",
              values_from = "value")

plot_rot <- plot_ly(df_rot_wide, x = ~X, y = ~Y, z = ~Z, color = ~factor(sensor_id), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = 'X'),
    yaxis = list(title = 'Y'),
    zaxis = list(title = 'Z')
  ))

plot_rot

                 