test_that("hc_data_series", {


  data_list <- list(
    name = "London",
    data = c(3.9, 4.2, 5.7, 8.5, 11.9, 15.2, 17.0, 16.6, 14.2, 10.3, 6.6, 4.8)
  )

  highchart() |> hc_data_series(data_list)


  data_list <- list(
    list(
      name = "Tokyo",
      data = c(7.0, 6.9, 9.5, 14.5, 18.4, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6)
    ),
    list(
      name = "London",
      data = c(3.9, 4.2, 5.7, 8.5, 11.9, 15.2, 17.0, 16.6, 14.2, 10.3, 6.6, 4.8)
    )
  )

  highchart() |> hc_data_series(data_list)


  data <- abs(rnorm(5))
  highchart() |> hc_data_series(data)


  ht <- iris |> select(Species, Sepal.Length) |> hdtable()
  data_viz <- data_prep(ht$data, ht$dic, "species", "sepal_length", agg = "mean")
  data_viz <- colors_data(data_viz, color_by = "species")
  data_list <- hg_list(data_viz, "CatNum", "treemap")
  highchart() |>
    hc_chart(type = "treemap") |>
    hc_data_series(data_list)

})
