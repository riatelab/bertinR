
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bertin

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/riatelab/bertinR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/riatelab/bertinR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

bertin is a R package that allows to create a multitude of interactive thematic maps (symbols, bubble, choropleth, typologies, spikes, dot cartogram, links, flows, dot density maps, "points Bertin", tiles, discontinuities, "mushroom"... ). Based on the javascript library [bertin.js](https://github.com/neocarto/bertin) 1.1.0, this package allows to realize simply, in the syntax of R, vector maps by taking advantage of the modern web technologies (d3.js, d3.geo, d3.geoprojection...).

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
