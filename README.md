
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bertin

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/riatelab/bertinR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/riatelab/bertinR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

[`bertin.js`](https://github.com/neocarto/bertin) is an easy to use
JavaScript library mainly based on `D3.js` makes creating thematic maps
simple. The principle is to work with layers stacked on top of one
other. Much like in Geographic Information Software (GIS) software,
`bertin.js` displays layers with a specific hierarchy. The layer at
bottom are rendered and then followed by the layer right above it. Some
of the layers are used to display various components of a map, some of
common layers are: header, footer, graticule, outline, choro, typo,
prop, shadow, scalebar, text etc.

**`bertin` package is a wrapper around `bertin.js`**

## Installation

You can install the development version of `bertin` from
[GitHub](https://github.com/riatelab/bertin) with:

``` r
# install.packages("remotes")
remotes::install_github("riatelab/bertin")
```

## Example

``` r
library(bertin)
library(sf)
world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
                 layer = "world", quiet = TRUE)
bt_param(width = 800)|>
  bt_layer(data = world, fill = "#808080") |>
  bt_bubble(data = world, values = "pop", k = 20) |>
  bt_draw() |> 
  bt_save("map.svg")
```

![](map.svg)  
file: “map.svg”

[See examples here](https://riatelab.github.io/bertin)
