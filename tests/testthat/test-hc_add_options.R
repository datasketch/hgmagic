test_that("hc_add_options", {

  hc_result <- highchart() |>
    hc_add_series(
      data = c(29.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4)
    ) |>
   hc_add_options(viz = "line")
  hc_result

})
