hc_add_bar <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "bar"),
            dsopts_merge(..., categories = "axis")
  )

  # Common hc_chart setup
  hc <- hc |>
    hc_chart(type = ifelse(opts$bar_orientation == "ver", "column", "bar"))


  if (opts$bar_orientation == "hor") {
    title_axis_x <- opts$title_axis_y
    title_axis_y <- opts$title_axis_x
    opts$title_axis_x <- title_axis_x
    opts$title_axis_y <- title_axis_y
  }


  # Handle different hdtype scenarios with consolidated conditional logic
  if (hdtype == "CatNum") {
    opts$legend_show <- FALSE
    hc <- hc |> add_CatNum_features(data, opts)
  }

  if (hdtype == "CatCatNum") {
    hc <- hc |> add_CatCatNum_features_bar(data, opts)
  }
  if (hdtype == "CatNumNum") {
    hc <- hc |> add_CatNumNum_features(data, opts)
  }

  hc

}


add_CatNum_features <- function(hc, data, opts) {
  hc <- hc |>
    hc_data_series(data$data) |>
    hc_axis("x", categories = data$categories,
            type = "category", opts = opts) |>
    hc_axis(axis = "y", opts = opts) |>
    hc_add_legend(opts)
  hc
}


add_CatCatNum_features_bar <- function(hc, data, opts) {

  hc <- hc |>
    hc_data_series(data$data) |>
    hc_axis("x", categories = data$categories,
            type = "category") |>
    hc_axis("y")

  if (opts$bar_graph_type == "stacked") {
    stackingOption <- ifelse(opts$percentage, "percent", "normal")
    hc <- hc |> hc_add_options(viz = "bar", user_options = opts)

    if (opts$percentage) {
      hc <- hc |>
        hc_yAxis(maxRange = 100, max = 100)
    }
  }

  hc

  hc
}
