hc_add_bar <- function(hc, data, hdtype, ...) {

  opts <- dsopts_merge(..., categories = "bar")

  # Common hc_chart setup
  hc <- hc |>
    hc_chart(type = ifelse(opts$bar_orientation == "ver", "column", "bar"))

  # Handle different hdtype scenarios with consolidated conditional logic
  if (hdtype == "CatNum") {
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
    hc_data_series(data) |>
    hc_axis(axis = "x") |>
    hc_axis(axis = "y") |>
    hc_add_legend()
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
