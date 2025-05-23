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

bite_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv"))

ref_idx <- c(1,2,3)
bp_idx <- c(5,6,7)

bite_data_3d <- bite_data[[1]]

rotated <- define_coord(bite_data_3d, ref_idx, bp_idx)

sensor_data <- load_tsv(here("tests", "sample_data", "PLURAL02_RP.tsv"))

sensor_data_3d <- sensor_data[[1]]


