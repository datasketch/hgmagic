test_that("hc_axis", {
  hc_add_series(
    data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
    type = "spline"
  ) |>
    hc_axis("x") |>
    hc_axis("y")
})
