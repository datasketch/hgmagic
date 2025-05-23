test_that("hc_titles", {
  highchart() |>
    hc_add_series(
      data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
      type = "column"
    ) |>
    hc_titles(
      title = "Title example <span style='cursor: pointer;' title='Test tooltip.'>?</span>",
      subtitle = "subtitle example",
      caption = NULL
    )
})
