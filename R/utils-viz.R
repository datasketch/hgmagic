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

hc_add_pie <- function(hc, data, hdtype, ...) {

  opts <- dsopts_merge(..., categories = "pie")

  hc <- hc |>
    hc_chart(type = "pie") |>
    add_CatNum_features(data, opts, "pie")
  hc

}

hc_add_donut <- function(hc, data, hdtype, ...) {

  opts <- dsopts_merge(..., categories = "donut")

  hc <- hc |>
    hc_chart(type = "pie") |>
    add_CatNum_features(data, opts, "pie")
  hc

}


hc_add_line <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "line"),
            dsopts_merge(..., categories = "axis")
  )
  line_type <- if (opts$line_spline) "spline" else "line"

  # Common hc_chart setup
  hc <- hc |>
    hc_chart(type = line_type)


  # Handle different hdtype scenarios with consolidated conditional logic
  if (hdtype == "DatNum") {
    opts$legend_show <- FALSE
    hc <- hc |> add_DatNum_features(data, opts, 'line')
  }

  if (hdtype == "CatDatNum") {
    hc <- hc |> add_CatDatNum_features(data, opts, 'line')
  }

  if (hdtype == "DatNumNum") {
    hc <- hc |> add_DatNumNum_features(data, opts, 'line')
  }

  hc

}


hc_add_item <- function(hc, data, hdtype, ...) {

  hc <- hc |>
    hc_chart(type = "item") |>
    hc_add_series(
      layout= 'horizontal',
      data = data
    ) |>
    # hc_tooltip(useHTML = TRUE,
    #            formatter = JS(paste0("function () {return this.point.label;}")))|>
    hc_plotOptions(
      series = list(
        states = list(
          hover = list(
            brightness= 0.1
          )
        ))
    )
  hc

}


hc_add_treemap <- function(hc, data, hdtype, ...) {

  opts <- dsopts_merge(..., categories = "treemap")

  hc <- hc |>
    hc_chart(type = "treemap")


  # Handle different hdtype scenarios with consolidated conditional logic
  if (hdtype == "CatNum") {
    hc <- hc |> add_CatNum_features(data, opts, "treemap")
  }

  if (hdtype == "CatCatNum") {
    hc <- hc |> add_CatCatNum_features(data, opts, "treemap")
  }

  hc

}

hc_add_solid_gauge <- function(hc, data, hdtype) {
  if (hdtype == "Num") {
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = rev(c('#55BF3B', '#DDDF0D', '#DF5353')),
      stringsAsFactors = FALSE
    )

    hc <- hc |>
      hc_chart(type = "solidgauge") |>
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) |>
      hc_tooltip(enabled = FALSE) |>
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        #tickAmount = 0,
        min = 0,
        max = 1, # TODO: agregar opción a dsopts
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) |>
      hc_add_series(
        data = data,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      )
  }
  hc
}

add_CatNum_features <- function(hc, data, opts, viz) {

  if (viz == "treemap") {
    hc <- hc |>
      hc_data_series(data)
  } else {
    hc <- hc |>
      hc_data_series(data$data)
  }

  if (viz %in% c("bar", "column")) {
    hc <- hc |>
      hc_axis("x", categories = data$categories,
              type = "category", opts = opts) |>
      hc_axis(axis = "y", opts = opts)
  }

  hc <- hc |>
    hc_add_options(viz = viz, opts) |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) |>
    hc_add_legend(opts)
  hc
}


add_CatCatNum_features <- function(hc, data, opts, viz) {

  hc <- hc |>
    hc_data_series(data$data) |>
    hc_axis("x", categories = data$categories,
            type = "category", opts = opts) |>
    hc_axis("y", opts = opts) |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) |>
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


add_DatNum_features <- function(hc, data, opts, viz) {

  hc <- hc |>
    hc_axis("x",
            categories = data$categories,
            type = 'datetime', opts = opts) |>
    hc_axis("y", opts = opts) |>
    hc_data_series(
      data$data
    ) |>
    hc_add_options(viz = viz, opts) |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) |>
    hc_add_legend(opts)
  hc
}



add_CatDatNum_features <- function(hc, data, opts, viz) {
  hc <- hc |>
    hc_axis("x",
            categories = data$categories,
            type = 'datetime', opts = opts) |>
    hc_axis("y", opts = opts) |>
    hc_data_series(
      data$data
    ) |>
    hc_add_options(viz = viz, opts) |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) |>
    hc_add_legend(opts)
  hc
}

add_DatNumNum_features <- function(hc, data, opts, viz) {
  hc <- hc |>
    hc_chart(zoomType = 'xy') |>
    hc_axis("x", categories = data$categories,
            type = "datetime", opts = opts) |>
    hc_yAxis_multiples(
      list(title = list(text = opts$title_axis_y)),
      list(title = list(text = opts$title_axis_y2),
           opposite = TRUE)
    ) |>
    hc_tooltip(useHTML = TRUE, shared = TRUE) |>
    hc_data_series(data$data)
  hc
}
