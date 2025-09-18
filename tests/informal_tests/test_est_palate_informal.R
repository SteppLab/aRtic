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

rotated <- define_coord(bite_data_3d, ref_idx, bp_idx, flip_axis = F)

coord <- rotated[[1]]

base_rt <- rotated[[2]]

base_center <- rotated[[3]]

data_palate <- load_tsv(here("tests", "sample_data", "PLURAL02_PalateTrace.tsv"))

data <- data_palate[[1]]

palate_trace <- est_palate(data, coord, ref_idx, pl_idx, base_rt, base_center)

plot_ly() |>
  add_trace(
    type = "scatter3d",
    mode = "markers",
    x = palate_trace[, 1],
    y = palate_trace[, 2],
    z = palate_trace[, 3],
    marker = list(size = 2),
    name = "Original Data"
  )


palate_coords <- palate_trace |>
  dplyr::mutate(row_id = row_number(),
                time = row_id/100)

palate_coords |>
  ggplot() +
  aes(x = time,
      y = X) +
  geom_point() 
  
palate_coords |>
  ggplot() +
  aes(x = time,
      y = Y) +
  geom_point() 

palate_coords |>
  ggplot() +
  aes(x = time,
      y = Z) +
  geom_point()
