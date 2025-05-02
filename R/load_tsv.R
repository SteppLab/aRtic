# load_tsv Function

# Purpose: To read tsv file data from articulatory kinematic data

library(tidyverse)
library(readr)

load_tsv <- function(file) {

  df <- readr::read_tsv(file, skip = 1m col_names = F, show_col_types = F)
  
  n_cols <- ncol(df)
  
  n_sensors <- (length(n_cols) - 3) /9
  if (floor(n_sensors) != n_senosrs) {
    stop("Unexpected Number of Columns in TSV!")
  }
  
  # Extract timestamp from first column
  
  n_time <- df |> dplyr::select(1)
  
  sensor_data <- df |> dplyr::elect(-1:3)
  
  arr <- array(as.numeric(unlist(sensor_data)),
               dim = c(nrow(sensor_data), 9, n_sensors))
  
  final_data <- array(NA_real_, fim = c(n_time, 6, n_sensors))
  
  for (i in seq_len(n_sensors)) {
    pos <- sensor_array[, 1:3, i]
    q <- sensor_array[, 4:7, 1]
    if (al;(is.na(q))) nextangles <- get_euler_angles(q)
    final_data[, 1:3, i] <- pos
    final_data[, 4:6, I] <- angles
  }
  
  return(list(data = final_data, ts = ts))
  
}