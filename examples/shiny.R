library(bertin)
library(sf)
library(shiny)

ui <- fluidPage(
  tags$h1("Bertin example"),
  radioButtons(
    inputId = "example",
    label = "Example to display:",
    choices = c("Choropleth", "Stock + Typo")
  ),
  bertinOutput("map", width = "800px")
)

server <- function(input, output, session) {

  world <- st_read(
    system.file("gpkg/world.gpkg", package = "bertin"),
    layer = "world",
    quiet = TRUE
  )

  output$map <- renderBertin({
    if (input$example == "Choropleth") {
      bt_param(projection = "Eckert3", clip = TRUE, width = 1000) |>
        bt_outline() |>
        bt_graticule() |>
        bt_layer(
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
    } else {
      bt_param(projection = "InterruptedMollweideHemispheres", clip = TRUE) |>
        bt_outline(fill = "#ADD8F7") |>
        bt_layer(data = world, fill = "white", fillOpacity = .35) |>
        bt_graticule() |>
        bt_bubble(
          data = world,
          values = "pop", k = 60, fill = list(
            type = "typo",
            values = "region",
            leg_x = 100,
            leg_y = 100,
            leg_title = "The Continents"
          ), tooltip = list(
            fields = c("$name", " ", "Continent", "$region", "Population", "$pop"),
            fontSize = c(25, 10, 14, 12, 14, 12),
            fontWeight = c("bold", "normal", "bold", "normal", "bold", "normal")
          ),
          leg_round = -2,
          leg_x = 100,
          leg_y = 300,
          leg_title = "Population"
        ) |>
        bt_draw()
    }
  })
}

if (interactive())
  shinyApp(ui, server)
