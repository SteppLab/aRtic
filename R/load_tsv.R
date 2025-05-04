# load_tsv Function

# Purpose: To read tsv file data from articulatory kinematic data

library(tidyverse)
library(readr)

load_tsv <- function(file) {

  df <- readr::read_tsv(file, skip = 1, col_names = F, show_col_types = F)
  
  n_cols <- ncol(df)
  
  n_sensors <- (n_cols - 3)/9
  if (floor(n_sensors) != n_sensors) {
    stop("Unexpected Number of Columns in TSV!")
  }
  
  # Extract timestamp from first column
  
  n_time <- df |> dplyr::select(1)
  n_timev <- as.numeric(unlist(n_time))
  
  sensor_data <- df |> dplyr::select(-c(1:3))

  arr <- array(as.numeric(unlist(sensor_data)),
               dim = c(nrow(sensor_data), 9, n_sensors))
  
  final_data <- array(NA_real_, dim = c(length(n_timev), 7, n_sensors))
  
  for (i in seq_len(n_sensors)) {
    pos <- arr[, 3:5, i]
    q <- arr[, 6:9, i]
    
    final_data[, 1:3, i] <- pos
    final_data[, 4:7, i] <- q
  }
  
  return(list(data = final_data, n_time = n_timev))
  
}