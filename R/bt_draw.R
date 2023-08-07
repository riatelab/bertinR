#' Draw the map
#'
#' @param bertin A `bertin` configuration list, typically initiated with [bt_param()].
#' @param elementId elementId
#' @param width,height A numeric input in pixels.
#'
#' @importFrom htmlwidgets createWidget
#' @importFrom jsonlite toJSON
#'
#' @export
bt_draw <- function(bertin, width = NULL, height = NULL, elementId = NULL) {
  bertin$params$reverse = TRUE
  for(i in seq_along(bertin$layers)){
    old_names <- names(bertin$layers[[i]])
    ind <- (which(old_names == "data"))
    if(length(ind>0)){
      new_names <- old_names
      new_names[ind] <- "geojson"
      names(bertin$layers[[i]]) <- new_names
    }
  }
  x <- list(
    parameters = bertin
  )
  attr(x, "TOJSON_ARGS") <- list(dataframe = "rows", auto_unbox = TRUE, pretty = FALSE, sf = "geojson")

  htmlwidgets::createWidget(
    name = "draw",
    x = x,
    width = width,
    height = height,
    package = "bertin",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = "100%",
      viewer.defaultHeight = "100%",
      viewer.defaultWidth = "100%",
      knitr.figure = FALSE,
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = "500px",
      browser.fill = TRUE,
      viewer.suppress = FALSE,
      browser.external = TRUE,
      padding = 0
    )
  )
}



