
#' Shiny bindings for Bertin.js
#'
#' Output and render functions for using draw within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a draw
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name bertin-shiny
#'
#' @export
#'
#' @example examples/shiny.R
bertinOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "draw", width, height, package = "bertin")
}

#' @rdname bertin-shiny
#' @export
renderBertin <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, bertinOutput, env, quoted = TRUE)
}



#' Shiny proxy for bertin widget
#'
#' @param shinyId single-element character vector indicating the output ID of the
#'   chart to modify (if invoked from a Shiny module, the namespace will be added
#'   automatically).
#' @param session the Shiny session object to which the chart belongs; usually the
#'   default value will suffice.
#'
#' @return An object of class `bertinProxy`.
#' @export
#'
#' @importFrom shiny getDefaultReactiveDomain
#'
#' @name bertin-shiny-proxy
#'
#' @example examples/shiny-proxy.R
bertinProxy <- function(shinyId, session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("bertinProxy must be called from the server function of a Shiny app")
  }
  if (!is.null(session$ns) && nzchar(session$ns(NULL)) && substring(shinyId, 1, nchar(session$ns(""))) != session$ns("")) {
    shinyId <- session$ns(shinyId)
  }
  structure(
    list(
      session = session,
      id = shinyId,
      x = list()
    ),
    class = "bertinProxy"
  )
}

#' @param proxy A proxy object created with `bertinProxy()`.
#' @param ... Parameters to update.
#' @rdname bertin-shiny-proxy
#' @export
bt_proxy_update <- function(proxy, ...) {
  if (!"bertinProxy" %in% class(proxy))
    stop("This function must be used with a bertinProxy object")
  proxy$session$sendCustomMessage(
    type = "bertin-update",
    message = list(id = proxy$id, data = list(...))
  )
  proxy
}
