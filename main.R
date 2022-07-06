
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
  res$type <- "layer"
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


