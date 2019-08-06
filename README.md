#Power Grid Networking
##An R package for complex network analysis of cascading failures on power grids.

#Instructions
1. Down load or clone the repo
1. Open [R](https://cran.r-project.org/)/[Rstudio](https://www.rstudio.com/) and ensure that devtools has been installed
1. Run the following code `library(devtools); install_github("JonnoB/PowerGridNetworking")`
1. Load the Power Grid Networking package normally using `library(PowerGridNetworking")`
1. All functions have help files e.g `?PowerFlow`

The package is designed to run on power networks represented by `igraph` object so ensure that this package is installed.

The package is currently light on examples these will be added as development continues. Good example datasets to explore can be found at [Electricity Ten Year Statement (ETYS)](https://www.nationalgrideso.com/insights/electricity-ten-year-statement-etys) or [IEEE](https://icseg.iti.illinois.edu/power-cases/). A use case of this package can be found at [this repo](https://github.com/JonnoB/ProportionalLoading).

This Repo stores all the functions that I use in my PhD. 
The functions are for the manilpulation of graph objects in R for the purposes of understanding cascading blackouts in the power-grid.
The package is used with associated repos Proportional Loading, and others as they are made.
