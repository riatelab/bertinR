% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bt_export.R
\name{bt_save}
\alias{bt_save}
\title{Export to SVG}
\usage{
bt_save(bertin, filename, background)
}
\arguments{
\item{bertin}{map object}

\item{filename}{file}

\item{background}{background}
}
\value{
nothing is returned, a svg file is created
}
\description{
Export to SVG
}
\examples{
\dontrun{
library(bertin)
library(sf)
world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
                 layer = "world", quiet = TRUE)
map <- bt_layer(data = world, fill = "#808080") |>
bt_draw()
(myfile <- tempfile(fileext = ".svg"))
if (require("chromote")){
  bt_save(bertin = map, filename = myfile)
}
}
}
