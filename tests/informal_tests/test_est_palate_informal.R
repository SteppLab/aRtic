# Informal Testing of est_palate

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
source(".\\R\\est_palate.R")
source(".\\R\\norm_vec.R")
source(".\\R\\center.R")

bite_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv"))

ref_idx <- c(1,2,3)
bp_idx <- c(5,6,7)
pl_idx <- 8

bite_data_3d <- bite_data[[1]]

rotated <- define_coord(bite_data_3d, ref_idx, bp_idx)

coord <- rotated[[1]]

base_rt <- rotated[[2]]

base_center <- rotated[[3]]

data_palate <- load_tsv(here("tests", "sample_data", "PLURAL02_PalateTrace.tsv"))

data <- data_palate[[1]]

palate_trace <- est_palate(data, rotated_plane, ref_idx, pl_idx, rotation, center)

n_time <- dim(palate_trace)[1]

all_idx <- c(ref_idx, pl_idx)

palate_trace1 <- lapply(seq_along(all_idx), function(i) {
  s <- c(all_idx[i])
  df <- as.data.frame(palate_trace[, 1:3, s])
  colnames(df) <- c("X", "Y", "Z")
  df$Time <- 1:n_time
  df$Sensor <- paste0(s)
  return(df)
})

palate_df <- do.call(rbind, palate_trace1)

palate_df <- as.data.frame(palate_df)

plot_ly(palate_df, x = ~X, y = ~Y, z = ~Z, color = ~Sensor,
        type = "scatter3d", mode = "lines") 

tri_faces <- hull_faces$triang

hull_faces <- as.data.frame(hull_faces)

num_faces <- nrow(hull_faces)
i <- seq(0, by = 3, length.out = num_faces)
j <- i + 1
k <- i + 2

plot_ly(spline_df, x = ~X, y = ~Y, z = ~Z, type = "scatter3d",
        mode = "lines+markers",
              marker = list(size = 2))

td_hull <- spline[, 1:2]

hull <- concaveman(td_hull, concavity = 2)

plot(hull, type = "l")

plot_ly() %>%
  add_trace(
    type = "scatter3d",
    mode = "markers",
    x = palate_coords[, 1],
    y = palate_coords[, 2],
    z = palate_coords[, 3],
    #line = list(color = 'red', width = 4),
    name = "Smoothed Spline"
  )

plot_ly() %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    x = palate_coords[, 1],
    y = palate_coords[, 2],
    marker = list(size = 2),
    name = "Original Data"
  ) |>
  add_trace(x = hull[,1], y = hull[,2], 
            type = 'scatter', mode = 'lines', line = list(color = 'red', width = 4))
