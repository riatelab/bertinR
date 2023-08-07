library(bertin)
library(sf)
library(shiny)

ui <- fluidPage(
  tags$h1("Bertin example"),
  fluidRow(
    column(
      width = 4,
      sliderInput(
        inputId = "nbreaks",
        label = "nbreaks",
        value = 7,
        min = 4,
        max = 9
      ),
      selectInput(
        inputId = "colors",
        label = "colors",
        choices = c(
          "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
          "Spectral", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
          "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu",
          "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
        )
      )
    ),
    column(
      width = 8,
      bertinOutput("map", width = "800px")
    )
  )
)

server <- function(input, output, session) {

  world <- st_read(
    system.file("gpkg/world.gpkg", package = "bertin"),
    layer = "world",
    quiet = TRUE
  )

  output$map <- renderBertin({
    bt_param(projection = "Eckert3", clip = TRUE, width = 1000) |>
      bt_outline() |>
      bt_graticule() |>
      bt_layer(
        id = "mylayer",
        data = world,
        fill = list(
          type = "choro",
          values = "gdppc",
          nbreaks = 7,
          method = "quantile",
          colors = "RdYlGn",
          leg_round = -2,
          leg_title = "GDP per inh\n(in $)",
          leg_x = 100,
          leg_y = 200
        ), tooltip = c("$name", "$gdppc", "(current US$)")
      ) |>
      bt_draw()
  })

  observeEvent(input$nbreaks, {
    bertinProxy("map") %>%
      bt_proxy_update(
        id = "mylayer",
        attr = "fill",
        value = list(nbreaks = input$nbreaks),
        duration = 0
      )
  })

  observeEvent(input$colors, {
    bertinProxy("map") %>%
      bt_proxy_update(
        id = "mylayer",
        attr = "fill",
        value = list(colors = input$colors),
        duration = 200
      )
  })

}

if (interactive())
  shinyApp(ui, server)
