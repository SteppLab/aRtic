#Informal Rotate Testing

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
source(".\\R\\rotate.R")

bite_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv"))

ref_idx <- c(1,2,3)
bp_idx <- c(5,6,7)

bite_data_3d <- bite_data[[1]]

rotated <- define_coord(bite_data_3d, ref_idx, bp_idx)

coord <- rotated[[1]]

base_rt <- rotated[[2]]

base_center <- rotated[[3]]

sensor_data <- load_tsv(here("tests", "sample_data", "PLURAL02_RP.tsv"))

data <- sensor_data[[1]]

time <- sensor_data[[2]]

corrected <- rotate(data, coord, ref_idx, base_rt, base_center, time)
