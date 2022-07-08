
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bertin

<!-- badges: start -->

[![R-CMD-check](https://github.com/riatelab/bertinR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/riatelab/bertinR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of bertin is to …

## Installation

You can install the development version of bertin from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
remotes::install_github("riatelab/bertinR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bertin)
library(sf)
#> Linking to GEOS 3.9.0, GDAL 3.2.2, PROJ 7.2.1; sf_use_s2() is TRUE
mtq <- st_read(system.file("geojson/mtq.geojson", package = "bertin"), quiet = TRUE)
a <- bt_bubble(geojson = mtq, values = "POP", k = 60, tooltip = "$LIBGEO") |>
  bt_layer(geojson = mtq, fill = "#808080") |>
  bt_draw()
htmlwidgets::saveWidget(a, 'map.html')
```

[See the result here](map.html)
