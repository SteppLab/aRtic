README
================
Micah E. Hirsch, Ph.D. <mehirsch@bu.edu>
2025-06-25

# aRtic

**Version:** 0.2.0

This package is designed to preprocess articulatory kinematic data
collected from EMA systems. It is currently in beta, designed to support
primary use cases with limited functionality.

Further enhancements and expanded features are planned based on user
feedback. Ideal for users comfortable with scripting in R. A shiny app
may be developed in the future

## Installation

You can install the latest development version of the package from
Github.

``` r
install.packages("remotes") # if you don't have remotes installed
remotes::install_github("SteppLab/aRtic")
```

## Usage

Here is a basic example of how to use the package. For more information
on function syntax, please check the documentation under the /man
folder.

``` r
library(aRtic)

# Load Your Data
## load_tsv returns a list: a 3d array of the raw data from the recording and a vector with the time stamps from the recording.

bite_plane_data <- load_tsv("path_to_your_data.tsv") # Bite Plane Calibration Recording
sensor_data <- load_tsv("path_to_your_data.tsv") # Sensor Recording 
palate_trace_data <- load_tsv("path_to_your_data.tsv") # Palate Trace Recoridng (optional)

# Define referent, biteplane, and palate sensor indices
## For the current version, three referent sensors are required (2 on the left and right mastoids and 1 on the incisor)

ref_idx <- c(1, 2, 3)
bp_idx <- c(5, 6, 7)
pl_idx <- 8

# Define the coordinate plane from the Bite Plane Recording
coord <- define_coord(bite_plane_data[[1]], ref_idx, bp_idx)

# Rotate sensor data to be aligned with the coordinate plane and correct for head movement
corrected <- rotate(bite_plane_data[[1]], coord[[1]], ref_idx, coord[[2]], coord[[3]], bite_plan_data[[2]])

# Estimate X, Y, and Z coordinates of palate trace (optional)
## filtering and correct_mov are done in this function already
palate <- est_palate(corrected, coord[[1]], ref_idx, pl_idx, coord[[2]], coord[[3]])
```

# Feedback, Questions, and Contribution

This package is in active development.

If you have feature requests, bug reports, or questions, please either
open an issue on Github or email Micah at <mehirsch@bu.edu>.

If you have ideas and would like to contribute to the project, please
email Micah.

# License

MIT © 2025 SteppLab
