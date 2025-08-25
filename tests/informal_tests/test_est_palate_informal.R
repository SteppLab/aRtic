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

bite_data <- load_tsv("R:\\SteppLab3\\Projects\\Voice\\SAKE\\artic_recordings\\PLURAL18\\PLURAL18_BitePlane.tsv")

ref_idx <- c(1,2,3)
bp_idx <- c(5,6,7)
pl_idx <- 8

bite_data_3d <- bite_data[[1]]

rotated <- define_coord(bite_data_3d, ref_idx, bp_idx, flip = F)

coord <- rotated[[1]]

base_rt <- rotated[[2]]

base_center <- rotated[[3]]

data_palate <- load_tsv("R:\\SteppLab3\\Projects\\Voice\\SAKE\\artic_recordings\\PLURAL18\\PLURAL18_PalateTrace.tsv")

data <- data_palate[[1]]

palate_trace <- est_palate(data, coord, ref_idx, pl_idx, base_rt, base_center)



plot_ly(palate_trace, x = ~X, y = ~Y, z = ~Z, 
        #color = factor(sensor_id),
        type = "scatter3d", mode = "markers") 


|>
  add_trace(
    data = palate_trace,
    x = ~V1,
    y = ~V2,
    type = "scatter",
    mode = "markers",
    opacity = 0.6
  )

