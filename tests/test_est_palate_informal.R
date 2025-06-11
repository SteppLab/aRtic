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

bite_data <- load_tsv(here("tests", "sample_data", "PLURAL02_BitePlane.tsv"))

ref_idx <- c(1,2,3)
bp_idx <- c(5,6,7)
pl_idx <- 8

bite_data_3d <- bite_data[[1]]

rotated <- define_coord(bite_data_3d, ref_idx, bp_idx)

rotated_plane <- rotated[[1]]

rotation <- rotated[[2]]

center <- rotated[[3]]

data_palate <- load_tsv(here("tests", "sample_data", "PLURAL02_PalateTrace.tsv"))

data <- data_palate[[1]]

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

palate_df <- as.data.frame(palate_df) |>
  dplyr::filter(Sensor != 8)

base <- plot_ly(palate_df, x = ~X, y = ~Y,
        type = "scatter", mode = "markers") 

plot <- add_trace(base, data = spline_df, x = ~X, y = ~Y,
            type = "scatter", mode = "lines",
            line = list(color = 'black', width = 4),
            name = "Principal Curve") |>
  layout(scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z")),
         title = "Palate Trace with Principal Curve")

plot
