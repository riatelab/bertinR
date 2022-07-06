
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



sessionInfo()
