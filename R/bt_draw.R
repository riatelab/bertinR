#' Draw the map
#'
#' @param bertin bertin

#' @param elementId elementId
#'
#' @importFrom htmlwidgets createWidget
#' @importFrom jsonlite toJSON
#'
#' @export
bt_draw <- function(bertin, elementId = NULL) {
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
    # forward options using x
    x = list(
      message = as.character(
        toJSON(bertin, auto_unbox = T, pretty = F, sf = "geojson")
        )
    )


    # create widget
    htmlwidgets::createWidget(
      name = 'draw',
      x,
      # width = width,
      # height = height,
      package = 'bertin',
      elementId = elementId
    )
  }

#'
#' #' Shiny bindings for draw
#' #'
#' #' Output and render functions for using draw within Shiny
#' #' applications and interactive Rmd documents.
#' #'
#' #' @param outputId output variable to read from
#' #' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#' #'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#' #'   string and have \code{'px'} appended.
#' #' @param expr An expression that generates a draw
#' #' @param env The environment in which to evaluate \code{expr}.
#' #' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#' #'   is useful if you want to save an expression in a variable.
#' #'
#' #' @name draw-shiny
#' #'
#' #' @export
#' drawOutput <- function(outputId, width = '100%', height = '400px'){
#'   htmlwidgets::shinyWidgetOutput(outputId, 'draw', width, height, package = 'bertin')
#' }
#'
#' #' @rdname draw-shiny
#' #' @export
#' renderDraw <- function(expr, env = parent.frame(), quoted = FALSE) {
#'   if (!quoted) { expr <- substitute(expr) } # force quoted
#'   htmlwidgets::shinyRenderWidget(expr, drawOutput, env, quoted = TRUE)
#' }
#'
