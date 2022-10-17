#' Export to SVG
#'
#' @param bertin map object
#' @param filename file
#' @param background background
#'
#@importFrom chromote ChromoteSession
#' @importFrom htmlwidgets saveWidget
#'
#' @return nothing is returned, a svg file is created
#' @export
#'
#' @examples
#' library(bertin)
#' library(sf)
#' world <- st_read(system.file("gpkg/world.gpkg", package = "bertin"),
#'                  layer = "world", quiet = TRUE)
#' map <- bt_layer(data = world, fill = "#808080") |>
#' bt_draw()
#' (myfile <- tempfile(fileext = ".svg"))
#' if (require("chromote")){
#'   bt_save(bertin = map, filename = myfile)
#' }
bt_save <- function(bertin, filename, background){
  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop(
      "'chromote' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }




  file_url <- function(filename) {
    if (.Platform$OS.type == "windows") {
      paste0("file://", normalizePath(filename, mustWork = TRUE))
    } else {
      enc2utf8(paste0("file://", normalizePath(filename, winslash = "/",
                                               mustWork = TRUE)))
    }
  }
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html))
  session <- chromote::ChromoteSession$new()
  # on.exit(session$close())
  htmlwidgets::saveWidget(bertin, file = tmp_html)
  session$Page$navigate(file_url(tmp_html))
  eval <- paste0(
    "var el = document.getElementById('htmlwidget_container').firstElementChild;\n",
    "el.shadowRoot === null ? el.innerHTML : el.shadowRoot.innerHTML;"
  )
  # wait for page loading
  while(TRUE){
    # svg extraction
    svg <- session$Runtime$evaluate(eval)$result$value
    if(length(svg) != 0){break}
  }
  # add correct meta/header info
  repl <- paste0('\\1 xmlns="http://www.w3.org/2000/svg" ',
                 'xmlns:xlink="http://www.w3.org/1999/xlink" \\2')
  svg <- gsub(pattern = '^(.{6})(.*)$', replacement = repl, x = svg)
  # write the svg file
  fileConn <- file(filename)
  writeLines(svg, fileConn)
  close(fileConn)
  try(session$parent$close(), silent = TRUE)
  try(session$close(), silent = TRUE)

  return(invisible(NULL))
}
