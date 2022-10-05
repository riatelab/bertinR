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


clean_input <- function(res, type){
  res <- res[unlist(lapply(X = res, FUN = function(x){!is.name(x)}))]
  res$bertin <- NULL
  res$type <- type
  res
}

