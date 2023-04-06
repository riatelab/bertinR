#' Params
#'
#'
#' @eval my_params(c(
#' 'margin',
#' 'width',
#' 'projection',
#' 'extent',
#' 'background',
#' 'clip',
#' 'reverse'))
#'
#' @return A list of params
#' @export
#'
#' @examples
#' bt_param(margin = 10)
bt_param <- function(margin=1, width=1000, projection="Mercator", extent = NULL ,background=NULL,clip=FALSE, reverse=FALSE){
  res <- c(as.list(environment()))
  not_empty <- function(x){sum(nchar(x))>0}
  res <- res[unlist(lapply(X = res, FUN = not_empty))]
  res <- list(params=res)
  return(res)
}




#' Layer
#'
#' @eval my_params(c(
#'
#' 'bertin',
#' 'data',
#' 'fill',
#' 'stroke',
#' 'strokeLinecap',
#' 'strokeLinejoin',
#' 'strokeWidth',
#' 'strokeDasharray',
#' 'fillOpacity',
#' 'strokeOpacity',
#' 'symbol',
#' 'symbol_size',
#' 'symbol_shift',
#' 'symbol_iteration',
#' 'display',
#' 'tooltip'))
#'
#'
#' @return a bertin list
#'
#' @export
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "Winkel3") |>
#'   bt_layer(data = world, fill = "red") |>
#'   bt_draw()
bt_layer <-function(bertin, data, fill,
                    tooltip, strokeWidth=0.5,
                    strokeDasharray=NULL, stroke = "white",strokeLinecap="round", strokeLinejoin= "round", fillOpacity = 1,
                    strokeOpacity=1, symbol="circle", symbol_size = 5, symbol_shift=0,symbol_iteration=200, display = TRUE ) {
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "layer")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Tissot's indicatrix
#'
#'
#' @eval my_params(c(
#'
#'  'bertin',
#'  'step',
#'  'fill',
#'  'fillOpacity',
#'  'stroke',
#'  'strokeWidth',
#'  'strokeOpacity',
#'  'display'))
#'
#' @return map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(projection = "Robinson", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_layer(data = world, fill = "white", fillOpacity = .35) |>
#'   bt_tissot(step = 20) |>
#'   bt_draw()
bt_tissot <- function(bertin, step=20 , fill="#d91848", fillOpacity=0.5, stroke ="white",strokeWidth=1.5, strokeOpacity=1,display=TRUE ){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "tissot")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Mercator Tiles
#'
#' @eval my_params(c(
#'
#' 'bertin',
#' 'style',
#' 'zoomDelta',
#' 'tileSize',
#' 'opacity',
#' 'clip',
#' 'source',
#' 'increasetilesize',
#' 'display'))
#'
#'
#' @return map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' italy <- world[world$name == "Italy",]
#' bt_param(projection = "Mercator", clip = TRUE, extent = italy) |>
#'   bt_tiles(style = "worldphysical") |>
#'   bt_draw()
bt_tiles <- function(bertin, style="opentopomap",zoomDelta = 0, tileSize=512, opacity=1,clip, source= c(100,200),increasetilesize=1, display=1){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "tiles")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Texts
#'
#' @eval my_params(c(
#'
#' 'bertin',
#'  'text',
#'  'position',
#'  'fontSize',
#'  'fontFamily',
#'  'textDecoration',
#'  'fontWeight',
#'  'fontStyle',
#'  'margin',
#'  'anchor',
#'  'fill',
#'  'stroke',
#'  'frame_fill',
#'  'frame_stroke',
#'  'frame_strokeWidth',
#'  'frame_opacity',
#'  'display'))
#'
#'
#' @return map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(projection = "Mercator", clip = TRUE) |>
#'   bt_layer(data = world) |>
#'   bt_text(text = "This is my text", position = "bottomright",
#'           fontSize = 20, frame_stroke = "red", margin = 4) |>
#'   bt_draw()
bt_text <- function(bertin, text, position=c(100,200),fontSize=15,fontFamily="Roboto",
                    textDecoration="none", fontWeight = "normal", fontStyle = "none",
                    margin=0,anchor="start",fill="#474342",stroke="none",frame_fill="none",
                    frame_stroke= "none", frame_strokeWidth= 1, frame_opacity=1,display=TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "text")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Labels
#'
#' @eval my_params(c(
#'
#'  'bertin',
#'  'data',
#'  'values',
#'  'fill',
#'  'fontSize',
#'  'fontFamily',
#'  'textDecoration',
#'  'fontWeight',
#'  'fontStyle',
#'  'opacity',
#'  'halo',
#'  'halo_style',
#'  'display'))
#'
#'
#' @return map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(projection = "Mercator", clip = TRUE) |>
#'   bt_layer(data = world) |>
#'   bt_label(data = world, values = "name") |>
#'   bt_draw()
bt_label <- function(bertin, data, values, fill = "#474342" ,fontSize=10, fontFamily = "Roboto", textDecoration ="none", fontWeight ="normal",
                     fontStyle="normal", opacity=1, halo= FALSE, halo_style= c("white",4,0.5),display=TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "label")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Graticules
#'
#' @eval my_params(c(
#'
#'  'bertin',
#'  'stroke',
#'  'strokeDasharray',
#'  'strokeWidth',
#'  'step',
#'  'display'))
#'
#'
#' @return map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "Winkel3") |>
#'   bt_layer(data = world, fill = "red") |>
#'   bt_graticule(strokeWidth = 1.5, stroke  = "green", step  = 15)|>
#'   bt_draw()
bt_graticule <- function(bertin, stroke="white", strokeDasharray="none",strokeWidth=0.8, step= c(10,10),display=TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "graticule")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Shadow
#'
#' @eval my_params(c(
#'
#' 'bertin',
#' 'data',
#' 'fill',
#' 'dx',
#' 'dy',
#' 'stdDeviation',
#' 'opacity',
#' 'display'))
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(projection = "Eckert3", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_shadow(data = world) |>
#'   bt_layer(data = world, fill = "white") |>
#'   bt_draw()
bt_shadow <- function(bertin, data, dx=3, dy=3, stdDeviation=1.5, opacity=0.7, fill= "#35383d", display=TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "shadow")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Outline
#'
#'@eval my_params(c(
#'
#' 'bertin',
#' 'fill',
#' 'opacity',
#' 'stroke',
#' 'strokeWidth',
#' 'display'))
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' bt_outline(fill = "#ADD8F7", stroke = "#4269ad") |>
#'   bt_draw()
bt_outline <- function(bertin, fill="#add8f7", opacity=1, stroke="none", strokeWidth=1, diplay=TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "outline")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}


#' Waterlines
#'
#' @eval my_params(c(
#'
#'  'bertin',
#'  'data',
#'  'dist',
#'  'nb',
#'  'precision',
#'  'stroke',
#'  'strokeOpacity',
#'  'strokeWidth',
#'  'strokeDasharray',
#'  'strokeLinecap',
#'  'strokeLinejoin',
#'  'display'))
#'
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "NellHammer") |>
#'   bt_layer(data = world) |>
#'   bt_waterlines(data = world, dist = 3, nb = 4) |>
#'   bt_draw()
bt_waterlines <-function(bertin, data, dist=5, nb=3, precision=3, stroke="#5d81ba",strokeOpacity= c(1,0.1), strokeWidth=c(1.2,0.2), strokeDasharray="none",
                         strokeLinecap="round", strokeLineJoin="round", display=TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "waterlines")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
  ### do nothing
}





#' Header
#'
#' @eval my_params(c(
#'
#' 'bertin',
#' 'text',
#' 'anchor',
#' 'fontSize',
#' 'fill',
#' 'background',
#' 'backgroundOpacity',
#' 'display'))
#'
#'
#'
#' @return a map object
#' @export
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "Winkel3") |>
#'   bt_layer(data = world, fill = "red") |>
#'   bt_header(text = "Title") |>
#'   bt_draw()
bt_header <-function(bertin, text="", fontSize=20, fill= "#9e9696", background= "white", backgroundOpacity=1, display= TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "header")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Footer
#'
#' @eval my_params(c(
#'
#' 'bertin',
#' 'text',
#' 'anchor',
#' 'fontSize',
#' 'fill',
#' 'background',
#' 'backgroundOpacity',
#' 'display'))
#'
#'
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(margin = 10, width = 500, projection = "Winkel3") |>
#'   bt_layer(data = world, fill = "red") |>
#'   bt_footer(text = "Title") |>
#'   bt_draw()
bt_footer <- function(bertin, text="", fontSize=15,anchor="end",fill="#9e9696", background= "white", backgroundOpacity=1, display=1){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "footer")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Scalebar
#'
#' @eval my_params(c(
#'
#' 'bertin',
#' 'x',
#' 'y',
#' 'units',
#' 'display'))
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_outline(fill = "#ADD8F7") |>
#'   bt_layer(data = world) |>
#'   bt_scalebar() |>
#'   bt_draw()
bt_scalebar <- function(bertin, x="left", y="bottom", units="kilometers", display= TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "scalebar")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Geolines
#'
#'@eval my_params(c(
#'
#'  'bertin',
#'  'stroke',
#'  'strokeWidth',
#'  'strokeOpacity',
#'  'strokeDasharray',
#'  'strokeLinecap',
#'  'display'))
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(projection = "Eckert3", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_layer(data = world, fill = "white") |>
#'   bt_geolines() |>
#'   bt_draw()
bt_geolines <- function(bertin, stroke= "#020e21", strokeWidth=c(1.5,1.2,0.7), strokeDasharray=c("none",5,3), strokeLinecap="round", display= TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "geolines")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Hatch
#'
#' @eval my_params(c(
#'
#'  'bertin',
#'  'angle',
#'  'stroke',
#' 'strokeWidth',
#'  'strokeOpacity',
#'  'strokeDasharray',
#'  'spacing',
#'  'display'))
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(projection = "Eckert3", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_layer(data = world, fill = "white") |>
#'   bt_hatch() |>
#'   bt_draw()
bt_hatch <- function(bertin, angle=45, stroke="#786d6c", strokeWidth=2, strokeOpacity=1, strokeDasharray="none", spacing=8, display=TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "hatch")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}


#' Rhumbs
#'
#'  @eval my_params(c(
#'
#' 'bertin',
#'  'nb',
#'  'position',
#'  'stroke',
#'  'strokeWidth',
#'  'strokeOpacity',
#'  'strokeDasharray',
#'  'display'))
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_param(projection = "Eckert3", clip = TRUE) |>
#'   bt_outline() |>
#'   bt_layer(data = world, fill = "white") |>
#'   bt_rhumbs(position = c(370, 370), nb = 25) |>
#'   bt_draw()
bt_rhumbs <- function(bertin, nb=10, position, stroke="#786d6c", strokeWidth=2, strokeOpacity=1, strokeDasharray="none", display=TRUE){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "rhumbs")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Bubble
#'
#' @eval my_params(c(
#'
#'  'bertin',
#'  'data',
#'  'values',
#'  'k',
#'  'fixmax',
#'  'fill',
#'  'stroke',
#'  'strokeWidth',
#'  'fillOpacity',
#'  'iteration',
#'  'tooltip',
#'  'display',
#'
#'
#'  'leg_x',
#'  'leg_y',
#'  'leg_fill',
#'  'leg_stroke',
#'  'leg_strokeWidth',
#'  'leg_txtcol',
#'  'leg_title',
#'  'leg_round',
#'  'leg_divisor',
#'  'leg_fontSize',
#'  'leg_fontSize2'))
#'
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_layer(data = world, fill = "#808080") |>
#'   bt_bubble(data = world, values = "pop", k = 60, tooltip = "$name") |>
#'   bt_draw()
bt_bubble <-
  function(bertin,
           data,
           values,
           k = 50,
           fixmax,
           fill = "randomcolor",
           stroke = "white",
           strokeWidth = 0.5,
           fillOpacity = 1,
           dorling = FALSE,
           iteration = 200,
           tooltip,
           display = TRUE,
           leg_x,
           leg_y,
           leg_fill = "none",
           leg_stroke = "black",
           leg_strokeWidth = 0.8,
           leg_txtcol = "#363636",
           leg_title = values,
           leg_round,
           leg_divisor = 1,
           leg_fontSize = 14,
           leg_fontSize2 = 10) {
    res <- c(as.list(environment()))
  res <- clean_input(res, type = "bubble")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Square
#'
#' @eval my_params(c(
#'
#' 'bertin',
#'  'data',
#'  'values',
#'  'k',
#'  'fixmax',
#'  'fill',
#'  'stroke',
#'  'strokeWidth',
#'  'fillOpacity',
#'  'demers',
#'  'iteration',
#'  'tooltip',
#'  'display',
#'
#' 'leg_x',
#' 'leg_y',
#' 'leg_fill',
#' 'leg_stroke',
#' 'leg_strokeWidth',
#' 'leg_txtcol',
#' 'leg_title',
#' 'leg_round',
#' 'leg_divisor',
#' 'leg_fontSize',
#' 'leg_fontSize2'))
#'
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#'library(sf)
#'world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                layer = "world", quiet = TRUE)
#'bt_layer(data = world, fill = "#808080") |>
#' bt_square(data = world, values = "pop", k = 60, tooltip = "$name") |>
#' bt_draw()
bt_square <-function(bertin, data, values, k=50, fixmax, fill = "random color", stroke="white", strokeWidth= 0.5, fillOpacity=1, demers=FALSE, iteration=200, tooltip, display= TRUE,
                     leg_x,leg_y, leg_fill="none", leg_stroke="black", leg_strokeWidth=0.8, leg_txtco = "#363636", leg_title=values, leg_round, leg_divisor=1, leg_fontSize=14, leg_fontSize2=10){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "square")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Regular Bubble
#'
#' @eval my_params(c(
#'
#' 'bertin',
#'  'data',
#'  'values',
#'  'k',
#'  'fixmax',
#' 'fill',
#'  'stroke',
#'  'strokeWidth',
#'  'fillOpacity',
#'  'dorling',
#'  'iteration',
#'  'step',
#'  'tooltip',
#'  'display',
#'
#'
#' 'leg_x',
#' 'leg_y',
#' 'leg_fill',
#'  'leg_stroke',
#'  'leg_strokeWidth',
#'  'leg_txtcol',
#' 'leg_title',
#'  'leg_round',
#' 'leg_divisor',
#'  'leg_fontSize',
#' 'leg_fontSize2'))
#'
#' @return a map object
#' @export
#'
#' @examples
#'library(sf)
#'world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                 layer = "world", quiet = TRUE)
#'bt_layer(data = world, fill = "#808080") |>
#' bt_regularbubble(data = world,
#'                  values = "pop", step = 10,
#'                    k = 10, tooltip = "$value") |>
#'  bt_draw()
bt_regularbubble <- function(bertin, data, values, k =50, fixmax, fill= "random color", stroke= "white", strokeWidth=0.5, fillOpacity=1, dorling=FALSE,step=20, iteration=200, tooltip, display=TRUE,
                             leg_x, leg_y, leg_fill = "none", leg_stroke="black",leg_strokeWidth= 0.8, leg_txtcol = "#363636", leg_title= values, leg_round, leg_divisor=1, leg_fontSize= 14,
                             leg_fontSize2=10){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "regularbubble")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Regular Square
#'
#' @eval my_params(c(
#'
#' 'bertin',
#' 'data',
#'  'values',
#'  'k',
#' 'fixmax',
#' 'fill',
#' 'stroke',
#' 'strokeWidth',
#' 'fillOpacity',
#' 'demers',
#' 'iteration',
#' 'step',
#' 'tooltip',
#' 'display',
#'
#'
#' 'leg_x',
#' 'leg_y',
#' 'leg_fill',
#'  'leg_stroke',
#' 'leg_strokeWidth',
#' 'leg_txtcol',
#' 'leg_title',
#' 'leg_round',
#' 'leg_divisor',
#' 'leg_fontSize',
#' 'leg_fontSize2'))
#'
#' @return a map object
#' @export
#'
#' @examples
#'library(sf)
#'world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                layer = "world", quiet = TRUE)
#'bt_layer(data = world, fill = "#808080") |>
#'  bt_regularsquare(data = world,
#'                 values = "pop", step = 10,
#'                   k = 10, tooltip = "$value") |>
#'  bt_draw()
bt_regularsquare <- function(bertin, data, values, k=50, fixmax, fill=randomcolor, stroke="white",strokeWidth =0.5, fillOpacity=1, step=20, demers= FALSE, iteration=200, tooltip, display=TRUE,
                             leg_x,leg_y, leg_fill="none", leg_stroke="black", leg_strokeWidth= 0.8, leg_txtcol= "#363636", leg_title= values, leg_round, leg_divisor=1,
                             leg_fontSize=14 , leg_fontSize2= 10){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "regularsquare")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Regular Grid
#'
#' @eval my_params(c(
#'
#' 'bertin',
#' 'data',
#' 'step',
#' 'values',
#' 'operator',
#' 'geoprocessing',
#' 'blur',
#'
#' 'colors',
#' 'order',
#' 'col_missing',
#'  'txt_missing',
#' 'stroke',
#'  'strokeWidth',
#' 'fillOpacity',
#'  'tooltip',
#'
#'
#'  'leg_x',
#'  'leg_y',
#'  'leg_w',
#'  'leg_h',
#'  'leg_fill',
#'  'leg_stroke',
#'  'leg_strokeWidth',
#'  'leg_txtco',
#'  'leg_title',
#'  'leg_round',
#'  'leg_fontSize'))
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_layer(data = world, fill = "#808080") |>
#'   bt_regulargrid(data = world, values = "pop",
#'                  step = 20,
#'                 fill = list(
#'                    nbreaks = 6,
#'                    method = "quantile",
#'                    colors = "Blues"
#'                  ),
#'                  tooltip = "$value") |>
#'   bt_draw()
bt_regulargrid <- function(bertin, data, step =20, values, operator="sum", geoprocessing= "intersection", blur= 0, colors= "Tableau10", order, col_missing= "#f5f5f5", txt_missing= "No data",
                           stroke="white", strokeWidth= 0.5, fillOpacity=1, tooltip, leg_x, leg_y, leg_w=30, leg_h = 20, leg_fill="none", leg_stroke="black", leg_strokeWidth = 0.8, leg_txtcol= "#363636",
                           leg_title= values, leg_round, leg_fontSize= 10){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "regulargrid")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Dot cartogram
#'
#' @eval my_params(c(
#'
#'  'bertin',
#'  'data',
#'  'onedot',
#'  'nbmax',
#'  'iteration',
#'  'values',
#'  'radius',
#' 'span',
#'  'fill',
#'  'stroke',
#'  'strokeWidth',
#'  'fillOpacity',
#'  'tooltip',
#'  'display',
#'
#'
#'  'leg_x',
#'  'leg_y',
#'  'leg_fill',
#'  'leg_stroke',
#'  'leg_strokeWidth',
#'  'leg_txtcol',
#'  'leg_title',
#'  'leg_round',
#'  'leg_fontSize'))
#'
#'
#'
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_layer(data = world, fill = "#808080") |>
#'   bt_dotcartogram(data = world,
#'                   onedot = 100000000000,
#'                   iteration = 200, values = "gdp") |>
#'   bt_draw()
bt_dotcartogram <-function(bertin, data, onedot, nbmax=200, iteration=200, values, radius=4, span=0.5, fill=randomcolor, stroke= "white", strokeWidth = 0.5, fillOpacity = 1, tooltip, display= TRUE,
                           leg_x, leg_y, leg_fill="none", leg_stroke="black", leg_strokeWidth = 0.8, leg_txtcol = "#363636", leg_title=values, leg_round, leg_fontSize= 10){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "dotcartogram")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Mushroom
#'
#' @eval my_params(c(
#'
#' 'bertin',
#' 'data',
#' 'top_values',
#' 'bottom_values',
#' 'top_fill',
#' 'bottom_fill',
#' 'k',
#' 'stroke',
#' 'strokeWidth',
#' 'fillOpacity',
#' 'strokeOpacity',
#' 'top_tooltip',
#' 'bottom_tooltip',
#' 'display',
#'
#'
#' 'leg_x',
#' 'leg_y',
#' 'leg_fill',
#' 'leg_stroke',
#' 'leg_strokeWidth',
#' 'leg_txtcol',
#' 'leg_title',
#' 'leg_round',
#' 'leg_fontSize',
#' 'leg_fontSize2',
#' 'leg_top_txt',
#' 'leg_bottom_txt',
#' 'leg_bottom_fill',
#' 'leg_top_fill'))
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' africa <- subset(world, region == "Africa")
#' africa$pop_pct <- (africa$pop / sum(africa$pop, na.rm = TRUE)) * 100
#' africa$gdp_pct <- (africa$gdp / sum(africa$gdp, na.rm = TRUE)) * 100
#' bt_layer(data = africa, fill = "#808080") |>
#'   bt_mushroom(data = africa, top_values = "gdp_pct", bottom_values = "pop_pct") |>
#'   bt_draw()
bt_mushroom <- function(bertin, data, top_values, bottom_values, top_fill = "#d64f4f", bottom_fill= "#4fabd6", k=50, stroke= "white", strokeWidth=0.5, fillOpacity=1, strokeOpacity=1,
                        top_tooltip, bottom_tooltip, display= TRUE,  leg_x,  leg_y, leg_fill="none", leg_stroke = "black", leg_strokeWidth= 0.8, leg_txtcol= "#363636", leg_title= values,
                        leg_round, leg_fontSize=14, leg_fontSize2= 10, leg_top_txt = top_var, leg_bottom_txt= bottom_var, leg_bottom_fill, leg_top_fill){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "mushroom")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Spikes
#'
#' @eval my_params(c(
#'
#'  'bertin',
#'  'data',
#'  'values',
#'  'k',
#'  'w',
#'  'fill',
#'  'stroke',
#'  'strokeWidth',
#'  'fillOpacity',
#'  'tooltip',
#'  'display',
#'
#'
#'  'leg_x',
#'  'leg_y',
#'  'leg_fill',
#'  'leg_stroke',
#'  'leg_strokeWidth',
#'  'leg_txtcol',
#'  'leg_title',
#'  'leg_round',
#'  'leg_fontSize',
#'  'leg_fontSize2'))
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_layer(data = world, fill = "#808080") |>
#'   bt_spikes(data = world, values = "pop", k = 110, w = 6) |>
#'   bt_draw()
bt_spikes <- function(bertin, data, values, k=50, w=10, fill = "#a31d88", stroke = 0.7, strokeWidth= 0.7, fillOpacity=0.3, tooltip, display=TRUE,
                      leg_x,leg_y, leg_fill="none", leg_stroke= "black", leg_strokeWidth= 0.8, leg_txtcol = "#363636", leg_title= values, leg_round,
                      leg_fontSize=14, leg_fontSize2= 10){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "spikes")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

#' Smooth
#'
#'@eval my_params(c(
#'
#'  'bertin',
#'  'data',
#'  'values',
#'  'stroke',
#'  'strokeWidth',
#'  'strokeLinecap',
#'  'strokeLinejoin',
#'   'strokeDasharray',
#'  'fillOpacity',
#'  'strokeOpacity',
#'  'display',
#'
#'  'fill',
#'  'thresholds',
#'  'bandwidth',
#'  'colorcurve',
#'  'reverse',
#'  'remove',
#'  'clip',
#'
#' 'grid_step',
#' 'grid_blur',
#' 'grid_operator',
#' 'grid_geoprocessing'))
#'
#' @return a map object
#' @export
#'
#' @examples
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' bt_layer(data = world, fill = "#808080") |>
#'   bt_smooth(data = world, values = "pop",
#'             thresholds = 50,
#'             bandwidth = 25,
#'             colorcurve = 1) |>
#'   bt_draw()
bt_smooth <- function(bertin, data, values, stroke="white", strokeWidth= 0.5, strokeLinecap= "round", strokeDasharray= "none", fllOpacity=1, strokeOpacity=1,display= TRUE,
                      fill= "RdYlGn", thresholds=100, bandwidth= 5, colorcurve= 2, reverse= FALSE, remove=0, clip=FALSE, grid_step=20, grid_blur=0, grid_operator="sum", grid_geoprocessing= "intersection"){
  res <- c(as.list(environment()))
  res <- clean_input(res, type = "smooth")
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
