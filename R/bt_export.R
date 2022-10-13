#' Export to SVG
#'
#' @param bertin map object
#' @param file file
#' @param delay delay (in seconds) to wait before saving the SVG
#'
#' @importFrom chromote ChromoteSession
#' @importFrom htmlwidgets saveWidget
#'
#' @return a svg file
#' @export
#'
#' @examples
#' library(bertin)
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' map <- bt_layer(data = world, fill = "#808080") |>
#' bt_draw()
#' # bt_save_svg(bertin = map, file = "test.svg", delay = 0.5)
bt_save_svg <- function(bertin, file, background, delay){
  file_url <- function(filename) {
    if (.Platform$OS.type == "windows") {
      paste0("file://", normalizePath(filename, mustWork = TRUE))
    } else {
      enc2utf8(paste0("file://", normalizePath(filename, winslash = "/",
                                               mustWork = TRUE)))
    }
  }
  tmp_html <- tempfile('bt_save_svg', fileext = '.html', tmpdir = getwd())
  session <- chromote::ChromoteSession$new()
  on.exit(unlink(tmp_html))
  htmlwidgets::saveWidget(map, file = tmp_html)
  session$Page$navigate(file_url(tmp_html))
  Sys.sleep(delay)
  eval <- paste0(
    "var el = document.getElementById('htmlwidget_container').firstElementChild;\n",
    "el.shadowRoot === null ? el.innerHTML : el.shadowRoot.innerHTML;"
  )
  svg <- session$Runtime$evaluate(eval)$result$value
  cat(svg, file = file)
  invisible(file)
}
