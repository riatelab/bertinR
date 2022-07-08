
bt_param <- function(margin, width, projection, ...){
  # get fun args and values
  res <- c(as.list(environment()), list(...))
  not_empty <- function(x){sum(nchar(x))>0}
  res <- res[unlist(lapply(X = res, FUN = not_empty))]
  res <- list(params=res)
  return(res)
}

bt_layer <-function(bertin, geojson, fill,
                    tooltip, strokeWidth,
                    strokeDasharray, stroke,step, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "layer")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

bt_graticule <- function(bertin, strokeWidth, strokeDasharray,
                        stroke, step, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "graticule")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

bt_waterlines <-function(bertin, geojson, dist, nb, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "waterlines")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

bt_header <-function(bertin, text, fontsize, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "header")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

bt_footer <- function(bertin, text, fontSize, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "footer")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

bt_bubble <-function(bertin, geojson, values, k, tooltip, ...){
  res <- c(as.list(environment()), list(...))
  res <- clean_input(res, type = "bubble")
  if(missing(bertin)){bertin <- list()}
  bertin$layers[[length(bertin$layers) + 1]] <- res
  return(bertin)
}

bt_toJSON <- function(x){
  res <- jsonlite::toJSON(x, auto_unbox = T, pretty = F, sf = "geojson")
  res
}

bt_export <- function(x, filename = "test.json"){
  jsonlite::write_json(x, filename)
}

clean_input <- function(res, type){
  res <- res[unlist(lapply(X = res, FUN = function(x){!is.name(x)}))]
  res$bertin <- NULL
  res$type <- type
  res
}

