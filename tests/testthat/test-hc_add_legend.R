test_that("hc_add_legend", {
  highchart() |>
    hc_xAxis(categories = month.abb)  |>
    hc_add_series(name = "Tokyo", data = sample(1:12))  |>
    hc_add_series(name = "London", data = sample(1:12) + 10)  |>
    hc_add_series(name = "Other City", data = sample(1:12) + 20) |>
    hc_add_legend()
})
