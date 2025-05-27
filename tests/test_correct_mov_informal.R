library(tidyverse)
library(readr)
library(plotly)
library(abind)
library(here)
library(pracma)
library(signal)
source(".\\R\\load_tsv.R")
source(".\\R\\euler.R")
source(".\\R\\define_coord.R")
source(".\\R\\rotation_matrix.R")
source(".\\R\\interp_filter.R")
source(".\\R\\correct_mov.R")

bite_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv"))

ref_idx <- c(1,2,3)
bp_idx <- c(5,6,7)

bite_data_3d <- bite_data[[1]]

rotated <- define_coord(bite_data_3d, ref_idx, bp_idx)

sensor_data <- load_tsv(here("tests", "sample_data", "PLURAL02_RP.tsv"))

sensor_data_3d <- sensor_data[[1]]

interpolated <- interp_filter(sensor_data_3d, ref_idx)

corrected <- correct_mov(rotated, interpolated, ref_idx, bp_idx)


coord_av <- apply(rotated, c(2, 3), mean, na.rm = T)
original <- apply(interpolated, c(2,3), mean, na.rm = T)
corrected <- apply(corrected, c(2,3), mean, na.rm = T)
ref_pos <- t(coord_av[1:3, ])  # 8 x 3
org_pos <- t(original[1:3, ])
mov_pos <- t(corrected[1:3, ])  # Last frame's positions

ref_df <- data.frame(ref_pos, sensor = factor(1:8), type = "ref")
orig_df <- data.frame(org_pos, sensor = factor(1:8), type = "org")
mov_df <- data.frame(mov_pos, sensor = factor(1:8), type = "mov")
colnames(ref_df)[1:3] <- colnames(mov_df)[1:3] <- colnames(orig_df)[1:3] <- c("X", "Y", "Z")


plot_ly() %>%
  add_markers(data = ref_df, x = ~X, y = ~Y, z = ~Z, color = ~type, symbol = ~sensor) %>%
  add_markers(data = orig_df, x = ~X, y = ~Y, z = ~Z, color = ~type, symbol = ~sensor) %>%
  add_markers(data = mov_df, x = ~X, y = ~Y, z = ~Z, color = ~type, symbol = ~sensor) %>%
  layout(scene = list(xaxis = list(title = 'X'),
                      yaxis = list(title = 'Y'),
                      zaxis = list(title = 'Z')))
