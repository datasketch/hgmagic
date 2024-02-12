test_that("hc_add_tooltip", {
  highchart() |>
    hc_add_series(data = sample(1:12)) |>
    hc_add_series(data = sample(1:12) + 10) |>
    hc_add_tooltip()
})
