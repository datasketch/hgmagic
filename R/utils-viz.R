hc_add_bar <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "bar"),
            dsopts_merge(..., categories = "axis")
  )
  bar_type <- if (opts$bar_orientation == "ver") "column" else "bar"

   # Common hc_chart setup
  hc <- hc |>
    hc_chart(type = bar_type)


  if (opts$bar_orientation == "hor") {
    title_axis_x <- opts$title_axis_y
    title_axis_y <- opts$title_axis_x
    opts$title_axis_x <- title_axis_x
    opts$title_axis_y <- title_axis_y
  }

  # Handle different hdtype scenarios with consolidated conditional logic
  if (hdtype == "CatNum") {
    opts$legend_show <- FALSE
    hc <- hc |> add_CatNum_features(data, opts, bar_type)
  }

  if (hdtype == "CatCatNum") {
    hc <- hc |> add_CatCatNum_features(data, opts, bar_type)
  }
  if (hdtype == "CatNumNum") {
    hc <- hc |> add_CatNumNum_features(data, opts, bar_type)
  }

  hc

}


add_CatNum_features <- function(hc, data, opts, viz) {
  hc <- hc |>
    hc_data_series(data$data) |>
    hc_axis("x", categories = data$categories,
            type = "category", opts = opts) |>
    hc_axis(axis = "y", opts = opts) |>
    hc_add_options(viz = viz, opts) |>
    hc_add_legend(opts)
  hc
}


add_CatCatNum_features <- function(hc, data, opts, viz) {

  hc <- hc |>
    hc_data_series(data$data) |>
    hc_axis("x", categories = data$categories,
            type = "category", opts = opts) |>
    hc_axis("y", opts = opts) |>
    hc_add_options(viz = viz, opts)

  hc
}

add_CatNumNum_features <- function(hc, data, opts, viz) {
  hc <- hc |>
    hc_chart(zoomType = 'xy') |>
    hc_axis("x", categories = data$categories,
            type = "category", opts = opts) |>
    hc_yAxis_multiples(
      list(title = list(text = opts$title_axis_y)),
      list(title = list(text = opts$title_axis_y2),
           opposite = TRUE)
    ) |>
    hc_tooltip(useHTML = TRUE, shared = TRUE) |>
    hc_data_series(data$data)
  hc
}
