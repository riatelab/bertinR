
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
  # get fun args and values
  res <- c(as.list(environment()), list(...))
  not_empty <- function(x){sum(nchar(x))>0}
  res <- res[unlist(lapply(X = res, FUN = not_empty))]
  res$bertin <- NULL
  # manage un set object
  if(missing(bertin)){
    bertin <- vector("list")
  }
  # manage later position
  if(!"layers" %in% names(bertin)){
    lb <- 1
  }else{
    lb <- length(bertin$layers) + 1
  }
  # use fixed type
  res$type <- "layer"
  bertin$layers[[lb]] <- res
  return(bertin)
}


bt_graticule <-function(bertin, strokeWidth, strokeDasharray,
                        stroke, step, ...){
  # get fun args and values
  res <- c(as.list(environment()), list(...))
  not_empty <- function(x){sum(nchar(x))>0}
  res <- res[unlist(lapply(X = res, FUN = not_empty))]
  res$bertin <- NULL

  # manage un set object
  if(missing(bertin)){
    bertin <- vector("list")
  }
  # manage first later
  if(!"layers" %in% names(bertin)){
    lb <- 1
  }else{
    lb <- length(bertin$layers) + 1
  }
  # use fixed type
  res$type <- "graticule"
  bertin$layers[[lb]] <- res
  return(bertin)
}


bt_waterlines <-function(bertin, geojson, dist, nb, ...){
  # get fun args and values
  res <- c(as.list(environment()), list(...))
  not_empty <- function(x){sum(nchar(x))>0}
  res <- res[unlist(lapply(X = res, FUN = not_empty))]
  res$bertin <- NULL
  # manage un set object
  if(missing(bertin)){
    bertin <- vector("list")
  }
  # manage later position
  if(!"layers" %in% names(bertin)){
    lb <- 1
  }else{
    lb <- length(bertin$layers) + 1
  }
  # use fixed type
  res$type <- "waterlines"
  bertin$layers[[lb]] <- res
  return(bertin)
}

bt_header <-function(bertin, text, fontsize, ...){
  # get fun args and values
  res <- c(as.list(environment()), list(...))
  not_empty <- function(x){sum(nchar(x))>0}
  res <- res[unlist(lapply(X = res, FUN = not_empty))]
  res$bertin <- NULL
  # manage un set object
  if(missing(bertin)){
    bertin <- vector("list")
  }
  # manage later position
  if(!"layers" %in% names(bertin)){
    lb <- 1
  }else{
    lb <- length(bertin$layers) + 1
  }
  # use fixed type
  res$type <- "header"
  bertin$layers[[lb]] <- res
  return(bertin)
}



bt_toJSON <- function(x){
  res <- jsonlite::toJSON(x, auto_unbox = T, pretty = T, sf = "geojson")
  res
}

bt_export <- function(x, filename = "test.json"){
  jsonlite::write_json(x, filename)
}




