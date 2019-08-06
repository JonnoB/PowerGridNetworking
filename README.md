# Power Grid Networking
## An R package for complex network analysis of cascading failures on power grids.

The R package is a general set of functions that can be used for DC simulation of cascading failures on the power grid. It also includes functions for simulating targeted and random attacks on a given power network. The package provides support for networks with Line limits as well as functions for generating line limits using either proportional loading or a linear models derived from real line limit data.

# Instructions
1. Open [R](https://cran.r-project.org/)/[Rstudio](https://www.rstudio.com/) and ensure that `devtools` has been installed
1. Run the following code `library(devtools); install_github("JonnoB/PowerGridNetworking")`
1. Load the Power Grid Networking package normally using `library(PowerGridNetworking)`
1. All functions have help files e.g `?PowerFlow`

The package can also be downloaded or cloned then installed locally using the `install` function from `devtools`.

# Comments
The package is designed to run on power networks represented by `igraph` object so ensure that this package is installed.

The package is currently light on examples these will be added as development continues. Good example datasets to explore can be found at [Electricity Ten Year Statement (ETYS)](https://www.nationalgrideso.com/insights/electricity-ten-year-statement-etys) or [IEEE](https://icseg.iti.illinois.edu/power-cases/). A use case of this package can be found at [this repo](https://github.com/JonnoB/ProportionalLoading).

