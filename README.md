
# efdcr

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.com/hxfan1227/efdcr.svg?branch=master)](https://travis-ci.com/hxfan1227/efdcr)
<!-- badges: end -->

The `efdcr` package is a toolbox in R for pre- and post-processing the model results of the Environmental Fluid Dynamics Code (EFDC). 
It provides tools to link existing EFDC models (build by EEMS) with your modelling wokflows in R.
`efdcr` enables you to set up the model data before you can directly import to the EEMS and to easily visualize the model results exported by EEMS. The central goal of `efdcr` is to return simulation results in a *tidy* format to facilitate an easy implementation of EFDC simulations, together with other R packages into clean and efficient R programming workflows. The `efdcr` pakcage is part of my PhD thesis and was for personal use origninally and the APIs are not stable. Inspired by the `SWATplusR` package, I decided to continue to develop the package. Any contributions are welcomed and appreciated. 


## Why `efdcr`?

- concise syntax: easy to memory and use
- work well with the `tidyverse` packages
- feature rich compared to the EEMS 
- careful API lifecycle management

## Installation

`efdcr` is still under development and will be constantly updated (more features will be added). 
You can Install the latest development version of efdcr from [GitHub](https://www.github.com) with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("hxfan1227/efdcr")
```

Installation from GitHub does not include the vignettes by default because they take some time to build.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(efdcr)
# use has_variable() to check wether the .nc file has variables

has_variable('water_quality.nc')
#> The file has 67 variables

# use get_efdc_nc_dt() to extract variables in the .nc file

dye_dt <- get_efdc_nc_dt('G:/water_quality.nc', var_name = 'DYE')

#> Reading bottom elevation (ZBOT)...
#> Finish reading bottom elevation (ZBOT)!
#> Reading water surface elevation (WSEL)...
#> Finish reading water surface elevation (WSEL)
#> Reading variables DYE ...
#> Finish reading variables DYE !

```

