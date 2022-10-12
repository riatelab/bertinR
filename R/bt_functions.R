#' Params
#'
#' @param margin margin
#' @param width width
#' @param projection proj
#' @param ... other params
#'
#' @return A list of params
#' @export
#'
#' @examples
#' bt_param(margin = 10)
bt_param <- function(margin, width, projection, ...){
  res <- c(as.list(environment()), list(...))
  not_empty <- function(x){sum(nchar(x))>0}
  res <- res[unlist(lapply(X = res, FUN = not_empty))]
  res <- list(params=res)
  return(res)
}




#' Layer
#'
#' @param bertin map object
#' @param geojson sf object EPSG:4326
#' @param fill fill
#' @param tooltip tooltip
#' @param strokeWidth stroke width
#' @param strokeDasharray stroke dash array
#' @param stroke stroke
#' @param step step
#' @param ... other params
#'
#' @return a bertin list
#'
#' @export
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("geojson/mtq.geojson", package = "bertin"), quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "Winkel3") |>
#'   bt_layer(geojson = mtq, fill = "red") |>
#'   bt_draw()
bt_layer <-function(bertin, geojson, fill,
                    tooltip, strokeWidth,
                    strokeDasharray, stroke,step, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "layer")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Tissot's indicatrix
#'
#' @param bertin map object
#' @param step step between circles
#' @param fill fill
#' @param fillOpacity fill opacity
#' @param stroke stroke
#' @param strokeWidth stroke width
#' @param strokeOpacity stroke opacity
#' @param ... other param
#'
#' @return map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("geojson/world.geojson", package = "bertin"))
#' bt_param(projection = "Robinson", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_layer(geojson = world, fill = "white", fillOpacity = .35) |>
#'   bt_tissot(step = 20) |>
#'   bt_draw()
bt_tissot <- function(bertin, step, fill, fillOpacity,
                      stroke, strokeWidth, strokeOpacity, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "tissot")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Graticules
#'
#' @param bertin map object
#' @param strokeWidth str wid
#' @param strokeDasharray str dash arr
#' @param stroke str
#' @param step step
#' @param ... other param
#'
#' @return map object
#' @export
#'
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("geojson/mtq.geojson", package = "bertin"), quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "Winkel3") |>
#'   bt_layer(geojson = mtq, fill = "red") |>
#'   bt_graticule(strokeWidth = 1.5, stroke  = "green", step  = 0.1)|>
#'   bt_draw()
bt_graticule <- function(bertin, strokeWidth, strokeDasharray,
                         stroke, step, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "graticule")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Shadow
#'
#' @param bertin map object
#' @param geojson geojson
#' @param dx dx
#' @param dy dy
#' @param stdDeviation blur
#' @param opacity opacity
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("geojson/world.geojson", package = "bertin"))
#' bt_param(projection = "Eckert3", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_shadow(geojson = world) |>
#'   bt_layer(geojson = world, fill = "white") |>
#'   bt_draw()
bt_shadow <- function(bertin, geojson, dx, dy, stdDeviation, opacity, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "shadow")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Outline
#'
#' @param bertin map object
#' @param fill fill
#' @param opacity opacity
#' @param stroke str
#' @param strokeWidth str wid
#' @param step step
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' bt_outline(fill = "#ADD8F7", stroke = "#4269ad") |>
#'   bt_draw()
bt_outline <- function(bertin, fill, opacity, stroke, strokeWidth, step, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "outline")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Waterlines
#'
#' @param bertin map object
#' @param geojson sf
#' @param dist dist
#' @param nb nb
#' @param ...  other
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("geojson/mtq.geojson", package = "bertin"), quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "Winkel3") |>
#'   bt_layer(geojson = mtq, fill = "red") |>
#'   bt_waterlines(geojson = mtq, dist = 0.25, nb = 5)|>
#'   bt_draw()
bt_waterlines <-function(bertin, geojson, dist, nb, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "waterlines")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Header
#'
#' @param bertin map object
#' @param text text
#' @param fontSize font size
#' @param ... other param
#'
#' @return a map object
#' @export
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("geojson/mtq.geojson", package = "bertin"), quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "Winkel3") |>
#'   bt_layer(geojson = mtq, fill = "red") |>
#'   bt_header(text = "Title") |>
#'   bt_draw()
bt_header <-function(bertin, text, fontSize, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "header")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Footer
#'
#' @param bertin bertin
#' @param text text
#' @param fontSize fontSize
#' @param ... other
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("geojson/mtq.geojson", package = "bertin"), quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "Winkel3") |>
#'   bt_layer(geojson = mtq, fill = "red") |>
#'   bt_footer(text = "Title") |>
#'   bt_draw()
bt_footer <- function(bertin, text, fontSize, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "footer")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Scalebar
#'
#' @param bertin map object
#' @param x x
#' @param y y
#' @param units units
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("geojson/world.geojson", package = "bertin"))
#' bt_outline(fill = "#ADD8F7") |>
#'   bt_layer(geojson = world) |>
#'   bt_scalebar() |>
#'   bt_draw()
bt_scalebar <- function(bertin, x, y, units, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "scalebar")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Geolines
#'
#' @param bertin map object
#' @param stroke stroke
#' @param strokeWidth stroke width
#' @param strokeDasharray stroke dasharray
#' @param strokeLinecap stroke linecap
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("geojson/world.geojson", package = "bertin"))
#' bt_param(projection = "Eckert3", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_layer(geojson = world, fill = "white") |>
#'   bt_geolines() |>
#'   bt_draw()
bt_geolines <- function(bertin, stroke, strokeWidth, strokeDasharray, strokeLinecap, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "geolines")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Hatch
#'
#' @param bertin map object
#' @param angle orientation of lines
#' @param stroke stroke
#' @param strokeWidth stroke width
#' @param strokeOpacity stroke opacity
#' @param strokeDasharray stroke dasharray
#' @param spacing spacing between lines
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("geojson/world.geojson", package = "bertin"))
#' bt_param(projection = "Eckert3", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_layer(geojson = world, fill = "white") |>
#'   bt_hatch() |>
#'   bt_draw()
bt_hatch <- function(bertin, angle, stroke, strokeWidth, strokeOpacity, strokeDasharray, spacing, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "hatch")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}


#' Rhumbs
#'
#' @param bertin map object
#' @param nb number of lines
#' @param position position of the center in x and y
#' @param stroke stroke color
#' @param strokeWidth stroke width
#' @param strokeOpacity stroke opacity
#' @param strokeDasharray stroke dasharray
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("geojson/world.geojson", package = "bertin"))
#' bt_param(projection = "Eckert3", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_layer(geojson = world, fill = "white") |>
#'   bt_rhumbs(position = c(370, 370), nb = 25) |>
#'   bt_draw()
bt_rhumbs <- function(bertin, nb, position, stroke, strokeWidth, strokeOpacity, strokeDasharray, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "rhumbs")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Bubble
#'
#' @param bertin map obj
#' @param geojson sf obj
#' @param values values
#' @param k k
#' @param tooltip tooltip
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("geojson/mtq.geojson", package = "bertin"), quiet = TRUE)
#' bt_bubble(geojson = mtq, values = "POP", k = 60, tooltip = "$LIBGEO") |>
#'   bt_layer(geojson = mtq, fill = "#808080") |>
#'   bt_draw()
bt_bubble <-function(bertin, geojson, values, k, tooltip, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "bubble")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Square
#'
#' @param bertin map obj
#' @param geojson sf obj
#' @param values values
#' @param k k
#' @param tooltip tooltip
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("geojson/world.geojson", package = "bertin"))
#' stats <- read.csv(system.file("csv/data.csv", package = "bertin"))
#' data <- merge(world, stats,  by.x = "ISO3", by.y = "id")
#' bt_param(projection = "Eckert3", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_layer(geojson = world, fill = "white") |>
#'   bt_square(geojson = data,values = "pop", k = 60,
#'             tooltip = c("$NAMEen", "$pop", "(inh.)")) |>
#'   bt_draw()
bt_square <-function(bertin, geojson, values, k, tooltip, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "square")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Regular Bubble
#'
#' @param bertin map obj
#' @param geojson sf obj
#' @param step values
#' @param values k
#' @param tooltip tooltip
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("geojson/mtq.geojson", package = "bertin"), quiet = TRUE)
#' bt_regularbubble(geojson = mtq, values = "POP", step = 30, k = 40, tooltip = c("$LIBGEO","$POP")) |>
#'   bt_layer(geojson = mtq, fill = "#808080") |>
#'   bt_draw()
bt_regularbubble <- function(bertin, geojson, step, values, tooltip, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "regularbubble")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}


#' Dot cartogram
#'
#' @param bertin map obj
#' @param geojson sf obj
#' @param onedot dot value
#' @param iteration an integer to define the number of iteration for the Dorling method
#' @param values values
#' @param radius radius
#' @param span spacing between dots
#' @param fill fill color
#' @param tooltip tooltip
#' @param ... other param
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("geojson/world.geojson", package = "bertin"))
#' data <- read.csv(system.file("csv/data.csv", package = "bertin"))
#' bt_layer(geojson = world, fill = "#808080") |>
#'   bt_dotcartogram(geojson = merge(world, data,  by.x = "ISO3", by.y = "id"),
#'                   onedot = 200000000000, iteration = 200, values = "gdp") |>
#'   bt_draw()
bt_dotcartogram <-function(bertin, geojson, onedot, iteration, values, radius, span, fill, tooltip, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "dotcartogram")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

clean_input <- function(res, type){
  res <- res[unlist(lapply(X = res, FUN = function(x){!is.name(x)}))]
  res$bertin <- NULL
  res$type <- type
  res
}
