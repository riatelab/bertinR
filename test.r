mtq <- sf::st_transform(mapsf::mf_get_mtq(), 4326)

bt_layer(geojson = mtq) |>
  bt_toJSON() |>
  bt_export("test1.json")

bt_param(margin = 10, width = 500, projection = "Winkel3", other_undocumented_arg = "toto") |>
  bt_layer(geojson = mtq, fill = "red") |>
  bt_toJSON() |>
  bt_export("test2.json")


bt_param(margin = 10, width = 500, projection = "Winkel3") |>
  bt_layer(geojson = mtq, fill = "red", other_undocumented_arg = "toto") |>
  bt_toJSON() |>
  bt_export("test3.json")

bt_param(margin = 10, width = 500, projection = "Winkel3") |>
  bt_layer(
    geojson = mtq,
    fill = list(
      type = "choro",
      values = "CHOM",
      breaks = c(78.00, 350.75, 839.00, 1659.00, 10046.00),
      leg_round = -1,
      leg_title = "Taux de chômage\n. pers",
      leg_x = 410,
      leg_y = 10
    )
  ) |>
  bt_toJSON() |>
  bt_export("test4.json")


bt_param(margin = 10, width = 500, projection = "Winkel3", other_undocumented_arg = "toto") |>
  bt_layer(geojson = mtq, fill = "red") |>
  bt_graticule(strokeWidth = 1.5, strokeDasharray = 2, stroke  = "green", step  = 0.1)|>
  bt_toJSON() |>
  bt_export("test5.json")





bt_param(margin = 10, width = 500, projection = "Winkel3", other_undocumented_arg = "toto")|>
  bt_bubble(geojson = mtq, values = "POP", k = 60, tooltip = "$LIBGEO") |>
  bt_waterlines(geojson = mtq, dist = 0.25, nb = 5)|>
  bt_layer(geojson = mtq, fill = "#808080") |>
  bt_graticule(strokeWidth = 1.5, strokeDasharray = 2, stroke  = "green", step  = 0.1)|>
  bt_header(text = "What a Map!\nTrop stylée", fontSize = 40) |>
  bt_footer(text="Credit lines, super longsuper longsuper longsuper longsuper long", fontSize = 8) |>
  bt_toJSON() |>
  bt_export("test6.json")




library(sf)
x <- st_read(dsn = "/home/tim/Documents/prj/fisheye-example/data-raw/COMMUNE.shp")
x <- x[x$INSEE_REG=="11", ]
x <-st_transform(x, 4326)


bt_param(margin = 10, width = 900, projection = "Mercator") |>
  bt_bubble( geojson = x, values = "POPULATION", k = 60, tooltip = "$NOM_COM")|>
  bt_layer( geojson = x, fill = "#0969da")|>
  bt_graticule( strokeWidth = 1.5, strokeDasharray = 2, stroke  = "green", step  = 0.2)|>
  bt_header( text = "Population communale en IDF", fontSize = 40)|>
  bt_footer( text="INSEE, IGN, 2022", fontSize = 8)|>
  bt_toJSON()|>
  bt_export( "testx.json")
