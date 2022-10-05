
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bertin

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/riatelab/bertinR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/riatelab/bertinR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Bertin.js is an easy to use JavaScript library mainly based on D3.js
makes creating thematic maps simple. The principle is to work with
layers stacked on top of one other. Much like in Geographic Information
Software (GIS) software, Bertin.js displays layers with a specific
hierarchy. The layer at bottom are rendered and then followed by the
layer right above it. Some of the layers are used to display various
components of a map, some of common layers are: header, footer,
graticule, outline, choro, typo, prop, shadow, scalebar, text etc.

**`bertin` package is a wrapper around Bertin.js**

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
mtq <- st_read(system.file("geojson/mtq.geojson", package = "bertin"), quiet = TRUE)
a <- bt_bubble(geojson = mtq, values = "POP", k = 60, tooltip = "$LIBGEO") |>
  bt_layer(geojson = mtq, fill = "#808080") |>
  bt_draw()
htmlwidgets::saveWidget(a, 'map.html')
```

[See examples here](https://riatelab.github.io/bertin)
