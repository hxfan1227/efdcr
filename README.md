
# efdcr

<!-- badges: start -->
<!-- badges: end -->

**efdcr** is a toolbox in R for post-processing the model results of the Environmental Fluid Dynamics Code (EFDC).

## Why `efdcr`?

- concise syntax: easy to memory and use
- work well with the `tidyverse` packages
- feature rich compared to the EEMS 
- careful API lifecycle management

## Installation

You can Install the latest development version of efdcr from [GitHub](www.github.com) with:

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

