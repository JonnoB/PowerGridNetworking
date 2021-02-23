# Power Grid Networking
## An R package for complex network analysis of cascading failures on power grids.

The R package is a general set of functions for network analysis that can be used for DC simulation of cascading failures on the power grid for systems under attack. It provides the tools necessary to run reproducible simulations in accordance with the PEARL framework discussed in [Bourne et al. 2019](https://arxiv.org/abs/1907.12848). 
It also includes functions for simulating targeted and random attacks on a given power network. The package provides support for networks with Line limits as well as functions for generating line limits using either proportional loading or a linear models derived from real line limit data. A website of package documentation can be found at https://jonnob.github.io/PowerGridNetworking/index.html

# Instructions
1. Open [R](https://cran.r-project.org/)/[Rstudio](https://www.rstudio.com/) and ensure that `devtools` has been installed
1. Run the following code `library(devtools); install_github("JonnoB/PowerGridNetworking")`
1. Load the Power Grid Networking package normally using `library(PowerGridNetworking)`
1. All functions have help files e.g `?PowerFlow`

The package can also be downloaded or cloned then installed locally using the `install` function from `devtools`.

# Comments
The package contains three methods to create line limits

1. `Line_Limit_Volt_PF` a linear model that uses the Voltage and initial power flow to calculate line limits
1. `Line_Limit_PF` a linear model that uses the initial power flow only to calculate the line limits
1.  `Proportional_Loading` a function that simply multiplies the initial loading by a factor supplied by the user

There is also a function that allows the loading of Matpower files into R, `matpower_loader`. A broad selection of networks have been collected into a [single library](https://github.com/power-grid-lib/pglib-opf) by [IEEE PES Task Force on Benchmarks for Validation of Emerging Power System Algorithms](https://power-grid-lib.github.io/). These can be useful for network analysis of power grids.

The package has few examples. Good example datasets to explore can be found at [Electricity Ten Year Statement (ETYS)](https://www.nationalgrideso.com/insights/electricity-ten-year-statement-etys) or [IEEE](https://icseg.iti.illinois.edu/power-cases/). A use case of this package can be found at [this repo](https://github.com/JonnoB/ProportionalLoading), which was used for the analysis in the article [Bourne et al. 2019](https://arxiv.org/abs/1907.12848).

# Cite
To cite this package use 
  Bourne,J.  and O’Sullivan,A.  and Arcaute,E. 2019
  Don’t go chasing artificial waterfalls: Artificial line limits and cascading failures in power grids.
  Chaos: An Interdisciplinary Journal of Nonlinear Science 29:11 doi.org/10.1063/1.5115493

