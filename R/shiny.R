
#' Shiny bindings for Bertin.js
#'
#' Output and render functions for using Bertin within Shiny
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
#' @seealso \code{\link[=bertinProxy]{bertinProxy()}} to update an already generated map from the server.
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



#' @title Shiny proxy for bertin widget
#'
#' @description
#'  It allows you to modify specific attributes and styles without having to redraw the entire map.
#'   Not everything can be modified. Only the attributes underlined
#'   in [the online documentation](https://github.com/neocarto/bertin) are.
#'
#'
#' @param shinyId Single-element character vector indicating the output ID of the
#'   chart to modify (if invoked from a Shiny module, the namespace will be added
#'   automatically).
#' @param session The Shiny session object to which the chart belongs; usually the
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
#' @param id The layer id.
#' @param attr The attribute to change.
#' @param value The new value to give to the attribute.
#' @param delay The time before making the change.
#' @param duration The time of transition.
#' @param legend In some cases, changing the attribute requires changing the
#'  title of the legend. You can do this via the legend parameter.
#'
#' @rdname bertin-shiny-proxy
#' @export
bt_proxy_update <- function(proxy,
                            id,
                            attr,
                            value,
                            delay = NULL,
                            duration = NULL,
                            legend = NULL) {
  if (!"bertinProxy" %in% class(proxy))
    stop("This function must be used with a bertinProxy object")
  params <- list(
    id = id,
    attr = attr,
    value = value,
    delay = delay,
    duration = duration,
    legend = legend
  )
  params <- params[!vapply(params, is.null, FUN.VALUE = logical(1))]
  proxy$session$sendCustomMessage(
    type = "bertin-update",
    message = list(
      id = proxy$id,
      data = params
    )
  )
  proxy
}
