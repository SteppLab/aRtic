README
================
Micah E. Hirsch, Ph.D. <mehirsch@bu.edu>
2025-07-11

# aRtic

**Version:** 0.2.1 (Beta)

This package provides core functionality for preprocessing articulatory
kinematic data from EMA systems. It is currently in beta, designed to
support primary use cases with limited functionality.

Further enhancements and expanded features are planned based on user
feedback. Ideal for users comfortable with scripting with R. A Shiny app
may be developed in the future.

## Installation

``` r
install.packages("remotes") # if you do not have remotes installed
remotes::install_github("StreppLab/aRtic")
```

## Usage

Here is a basic example of how to use the package:

``` r
# Loading the Package and Dependencies
library(aRtic)
library(signal)
library(pracma)
library(tidyverse)
library(zoo)
library(abind)
library(readr)

# Load Data
## This function returns a list. The first object is the data and the second object are the timestamps.
bite_plane_data <- load_tsv("your_file_path")
sensor_rec_data <- load_tsv("your_file_path")
palate_trace_data <- load_tsv("your_file_path")

sensor_data <- sensor_rec_data[[1]]
time_stamps <- sensor_rec_data[[2]]
palate_data <- palate_trace_data[[1]]
bite_data <- bite_plane_data[[1]]

# Define referent, bite plane, and palate trace sensor idicies
## The current version of this package requires 3 referent sensors (two behind the left and right mastoids, one on the incisor)

ref_idx <- c(1, 2, 3)
bp_idx <- c(5, 6, 7)
pl_idx <- 8

# Defining the coordinate plane from the bite plane recording
## This function returns a list of objects. First is the rotated plane data matrix, the second is the rotation matrix, and the third is the translation vector.

coord <- define_coord(bite_plane_data[[1]], ref_idx, bp_idx)
coord_data <- coord[[1]]
base_rt <- coord[[2]]
base_center <- coord[[3]]

# Interpolation and filtering of sensor data recording
filtered <- interp_filter(sensor_data, ref_idx)

# Rotate and correct sensor recording for head movement
corrected <- rotate(filtered, coord_data, ref_idx, base_rt, base_center, time_stamps)

# Estimate palate location (optional)
## Filtering and rotation steps are built into this function

palate <- est_palate(palate_data, coord_data, ref_idx, pl_idx, base_rt, base_center)
```

Please see /man folder for more information

# Feedback and Contribution

This package is in active development.

If you have feature requests, bug reports, or questions, please open an
issue on GitHub or email Micah at <mehirsch@bu.edu>

**Report Issues Here:** <https://github.com/SteppLab/aRtic/issues>

# License

MIT © 2025 SteppLab
