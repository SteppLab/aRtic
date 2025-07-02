#' Rotate Sensor Recording Function (rotate)
#'
#' This function rotates 3D articulatory kinematic data from a  recording and 
#' converts it into a dataframe ready for analysis.
#' 
#' @param data A 3d array of the sensor recording data
#' @param coord A 3d array of the rotated bite plane data
#' @param ref_idx A vector of the numeric ids of the three referent sensors
#' @param rotation A rotation matrix extracted from define_coord
#' @param center A vector with a length of 3 representing the translation vector extracted from define_coord
#' @param time A vector representing the time stamps of the original sensor data recording
#' @return A data frame of the rotated data ready for analysis
#' @import dplyr pracma purrr
#' @export
#' 

rotate <- function(data, coord, ref_idx, rotaton, center, time) {
  
  # Interpolate and filter missing values
  filtered <- interp_filter(data, ref_idx)
  
  # rotate the data
  rotated <- correct_mov(fitlered, coord, ref_idx, rotation, center)
  
  # merge data with time stamp information
  all_idx <- dim(corrected)[3]
  n_time <- dim(corrected)[1]
  
  trajectory <- lapply(seq_along(all_idx), function(i) {
    s <- all_idx[i]
    df <- as.data.frame(corrected[, 1:3, s])
    colnames(df) <- c("X", "Y", "Z")
    df$Time <- 1:n_time
    df$Sensor <- paste0(s)
    return(df)
  })
  
  traj_df <- do.call(rbind, trajectory)
  
  time_stamps <- as.data.frame(time)
  
  sensors <- traj_df |>
    group_split(Sensor)
  
  merged <- map2(sensors, time_stamps, ~mutate(.x, time = .y))
  
  final_df <- bind_rows(merged)
  
  return(final_df)
  
}