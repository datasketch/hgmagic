hc_add_bar <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "bar"),
            dsopts_merge(..., categories = "axis")
  )
  opts_theme <-  dsopts_merge(..., categories = "theme")
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
    opts <- c(opts, dsopts_merge(..., categories = "legend"))
    hc <- hc |> add_CatCatNum_features(data, opts, bar_type)
  }
  if (hdtype == "CatNumNum") {
    hc <- hc |> add_CatNumNum_features(data, opts, bar_type)
  }

  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

  hc

}

hc_add_pie <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "pie"),
            dsopts_merge(..., categories = "legend"))
  opts_theme <-  dsopts_merge(..., categories = "theme")
  hc <- hc |>
    hc_chart(type = "pie") |>
    add_CatNum_features(data, opts, "pie")
  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

  hc

}

hc_add_donut <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "donut"),
            dsopts_merge(..., categories = "legend"))
  opts_theme <-  dsopts_merge(..., categories = "theme")
  hc <- hc |>
    hc_chart(type = "pie") |>
    add_CatNum_features(data, opts, "pie")
  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

  hc

}

hc_add_line <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "line"),
            dsopts_merge(..., categories = "axis"),
            dsopts_merge(..., categories = "legend")
  )
  opts_theme <-  dsopts_merge(..., categories = "theme")
  line_type <- if (opts$line_spline) "spline" else "line"

  # Common hc_chart setup
  hc <- hc |>
    hc_chart(type = line_type)


  # Handle different hdtype scenarios with consolidated conditional logic
  if (hdtype == "DatNum") {
    opts$legend_show <- FALSE
    opts$palette_colors <- data$color
    hc <- hc |> add_DatNum_features(data, opts, 'line')
  }

  if (hdtype == "CatDatNum") {
    opts <- c(opts, dsopts_merge(..., categories = "legend"))
    hc <- hc |> add_CatDatNum_features(data, opts, 'line')
  }

  if (hdtype == "DatNumNum") {
    hc <- hc |> add_DatNumNum_features(data, opts, 'line')
  }
  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))


  hc

}

hc_add_item <- function(hc, data, hdtype, ...) {
  opts_theme <-  dsopts_merge(..., categories = "theme")
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
  opts_theme <-  dsopts_merge(..., categories = "theme")
  hc <- hc |>
    hc_chart(type = "treemap")


  # Handle different hdtype scenarios with consolidated conditional logic
  if (hdtype == "CatNum") {
    hc <- hc |> add_CatNum_features(data, opts, "treemap")
  }

  if (hdtype == "CatCatNum") {
    hc <- hc |> add_CatCatNum_features(data, opts, "treemap")
  }

  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

  hc

}

hc_add_scatter <- function(hc, data, hdtype, ...) {

  opts <- c(
    dsopts_merge(..., categories = "scatter"),
    dsopts_merge(..., categories = "legend"),
    dsopts_merge(..., categories = "axis")
  )
  opts_theme <-  dsopts_merge(..., categories = "theme")
  hc <- hc |>
    hc_chart(zoomType = "xy")

  if (hdtype == "NumNum") {
    hc <- hc |> add_NumNum_features(data, opts, "scatter")
  }

  if (hdtype == "CatNumNum") {
    hc <- hc |> add_CatNumNum_features(data, opts, "scatter")
  }

  if (hdtype == "CatNumNumNum") {
    hc <- hc |> add_CatNumNumNum_features(data, opts, "scatter")
  }

  if (hdtype == "CatCatNum") {
    hc <- hc |> add_CatCatNum_features(data, opts, "scatter")
  }
  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

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

hc_add_sunburst <- function(hc, data, hdtype, ...){
  opts <- c(dsopts_merge(..., categories = "tooltip"),
            dsopts_merge(..., categories = "axis"),
            dsopts_merge(..., categories = "sunburst")
  )

  opts_theme <- dsopts_merge(..., categories = "theme")

  hc <- hc |>
    hc_chart(type = "sunburst")

  if (hdtype == "CatCatNum") {
    hc <- hc |> add_CatCatNum_features(data = data, opts = opts, viz = "sunburst")
  }

  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

  hc
}

hc_add_parallel_coordinates <- function(hc, data, hdtype, ...){
  opts <- c(dsopts_merge(..., categories = "axis"))

  hc <- hc |>
    hc_add_dependency("modules/parallel-coordinates.js") |>
    hc_add_dependency("modules/accesibility.js") |>
    hc_chart(
      type = "spline",
      parallelCoordinates =  TRUE,
      parallelAxes = list(
        lineWidth = 3
      )
    )

  if (hdtype == "CatCatCatCatCatCatCat") {
    hc <- hc |> add_CatCatCatCatCatCatCat_features(data, opts, "parallel_coordinates")
  }

  hc
}

hc_add_radial_bar <- function(hc, data, hdtype, ...) {

  opts <- c(
    dsopts_merge(..., categories = "bar"),
    dsopts_merge(..., categories = "axis")
  )
  opts_theme <-  dsopts_merge(..., categories = "theme")

  hc <- hc |>
    hc_chart(
      type = "column",
      inverted = TRUE,
      polar = TRUE
    )

  if (hdtype == "CatNum") {
    opts$legend_show <- FALSE
    hc <- hc |> add_CatNum_features(data, opts, "column")
  }

  if (hdtype == "CatCatNum") {
    opts <- c(opts, dsopts_merge(..., categories = "legend"))
    hc <- hc |> add_CatCatNum_features(data, opts, "column")
  }

  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

  hc

}

hc_add_sankey <- function(hc, data, hdtype, ...){
  opts <- c(dsopts_merge(..., categories = "axis"),
            dsopts_merge(..., categories = "sankey"))

  opts_theme <- dsopts_merge(..., categories = "theme")
  hc <- hc |>
    hc_chart(type = "sankey")

  if (hdtype == "CatCatNum") {
    hc <- hc |> add_CatCatNum_features(data, opts, "sankey")
  }

  if (hdtype == "CatCatCatNum") {
    hc <- hc |> add_CatCatCatNum_features(data, opts, "sankey")
  }
  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))
  hc
}

hc_add_dumbbell <- function(hc, data, hdtype, ...){
  opts <- c(dsopts_merge(..., categories = "axis"))

  hc <- hc |>
    hc_chart(type = "dumbbell", inverted = TRUE)

  if (hdtype == "CatNumNum") {
    hc <- hc |> add_CatNumNum_features(data, opts, "dumbbell")
  }

  hc
}

hc_add_bar_line <- function(hc, data, hdtype, ...) {

  opts <- c(dsopts_merge(..., categories = "bar"),
            dsopts_merge(..., categories = "line"),
            dsopts_merge(..., categories = "axis")
  )
  opts_theme <-  dsopts_merge(..., categories = "theme")
  if (hdtype == "CatNumNum") {
    hc <- hc |> add_CatNumNum_features(data, opts, "bar_line")
  }
  if (hdtype == "DatNumNum") {
    hc <- hc |> add_DatNumNum_features(data, opts, "bar_line")
  }
  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

  hc

}

hc_add_bar_grid <- function(hc, data, hdtype, ...) {

  opts <- c(
    dsopts_merge(..., categories = "bar"),
    dsopts_merge(..., categories = "axis")
  )
  opts_theme <-  dsopts_merge(..., categories = "theme")
  if (hdtype == "CatCatNum") {
    hc <- hc |> add_CatCatNum_features(data, opts, "bar_grid")
  }

  if (hdtype == "CatCatCatNum") {
    hc <- hc |> add_CatCatCatNum_features(data, opts, "bar_grid")
  }
  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

  hc

}

hc_add_bar_icons <- function(hc, data, hdtype, ...) {

  opts <- c(
    dsopts_merge(..., categories = "bar"),
    dsopts_merge(..., categories = "axis")
  )
  opts_theme <-  dsopts_merge(..., categories = "theme")
  if (hdtype == "CatImgNum") {
    hc <- hc |> add_CatImgNum_features(data, opts, "bar_icons")
  }
  hc <- hc |>
    hc_add_theme(hgch_theme(opts = opts_theme))

  hc
}

add_NumNum_features <- function(hc, data, opts, viz) {

  if (viz %in% c("scatter")) {
    colors <- unique(data$..colors)

    hc <- hc |>
      hc_chart(type = "scatter") |>
      hc_axis(axis = "x", opts = opts) |>
      hc_axis(axis = "y", opts = opts) |>
      hc_data_series(data) |>
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS("function () {return this.point.label;}")
      ) |>
      hc_legend(enabled = FALSE)
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
    hc_tooltip(
      useHTML = TRUE,
      formatter = JS("function () {return this.point.label;}")
    ) |>
    hc_add_legend(opts)

  hc
}

add_CatCatNum_features <- function(hc, data, opts, viz) {

  if (viz %in% c("bar", "column", "treemap")) {
    hc <- hc |>
      hc_data_series(data$data) |>
      hc_axis("x", categories = data$categories,
              type = "category", opts = opts) |>
      hc_axis("y", opts = opts) |>
      hc_tooltip(useHTML = TRUE,
                 formatter = JS(paste0("function () {return this.point.label;}"))) |>
      hc_add_options(viz = viz, opts) |>
      hc_add_legend(opts)
  }

  if (viz == "bar_grid") {
    names <- names(data)
    colors <- unique(data$..colors)

    categories <- data |>
      select(names[1:2]) |>
      group_by(name = !!sym(names[1])) |>
      summarise(categories = list(!!sym(names[2]))) |>
      list_parse()

    hc <- hc |>
      hc_add_series(
        name = names[3], data = data[[3]],
        type = "column", color = colors[1]
      ) |>
      hc_xAxis(
        categories = categories,
        labels = list(style = list(fontSize = "10px"))
      ) |>
      hc_add_dependency("plugins/grouped-categories.js")
  }

  if (viz == "scatter") {
    hc <- hc |>
      hc_axis(
        axis = "x", type = "category",
        categories = data$categories, opts = opts
      ) |>
      hc_axis(axis = "y", opts = opts) |>
      hc_data_series(data$data) |>
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS("function () {return this.point.label;}")
      ) |>
      hc_add_legend(opts = opts)
  }

  if (viz == "sankey"){
    hc <- hc |>
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0("function () {return this.point.label;}"))
      ) |>
      hc_add_series(
        data = data$data,
        keys = c('from', 'to', 'weight'),
        nodes = data$nodes,
        linkOpacity = opts$sankey_link_opacity,
        opacity = opts$sankey_node_opacity
      )
  }

  if (viz == "sunburst") {

    hc <- hc |>
      hc_chart(type = "sunburst") |>
      hc_colors(opts$color_center_sunburst)|>
      # hc_tooltip(useHTML = TRUE,
      #            formatter = JS(paste0("function () {return this.point.label;}"))) |>
      hc_series(
        list(
          data = data,
          cursor = "pointer",
          dataLabels = list(
            format = "{point.name}"
          ),
          levels = list(
            list(
              level = 1
            ),
            list(
              level = 2
            ),
            list(
              level = 3
            )
          )
        )
      )
  }

  hc
}

add_CatCatCatNum_features <- function(hc, data, opts, viz) {

  if (viz == "bar_grid") {
    names <- names(data)
    colors <- unique(data$..colors)

    categories <- data |>
      select(names[1:3]) |>
      group_by(name = !!sym(names[1]), !!sym(names[2])) |>
      summarise(
        categories = list(
          list(
            name = unique(!!sym(names[2])),
            categories = !!sym(names[3])
          )
        )
      ) |>
      group_by(name) |>
      summarise(
        categories = list(
          categories = categories
        )
      ) |>
      list_parse()

    hc <- hc |>
      hc_add_series(
        name = names[4], data = data[[4]],
        type = "column", color = colors[1]
      ) |>
      hc_xAxis(
        categories = categories,
        labels = list(style = list(fontSize = "10px"))
      ) |>
      hc_add_dependency("plugins/grouped-categories.js")
  }

  if (viz == "sankey"){

    hc <- hc |>
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0("function () {return this.point.label;}"))
      ) |>
      hc_add_series(
        data = data$data,
        keys = c('from', 'to', 'weight'),
        nodes = data$nodes,
        opacity = 0.8
      )
  }
  hc
}

add_CatCatCatCatCatCatCat_features <- function(hc, data, opts, viz) {
  #TODO hc_yAxis optimization
  if (viz == "parallel_coordinates") {

    hc <- hc |>
      hc_axis("x", categories = data$xAxis, opts = opts) |>
      hc_yAxis_multiples(
        data$yAxis[[1]], data$yAxis[[2]],
        data$yAxis[[3]], data$yAxis[[4]],
        data$yAxis[[5]], data$yAxis[[6]],
        data$yAxis[[7]]
      ) |>
      hc_data_series(data$data)|>
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS("function() {
        return this.series.userOptions.label;
                       }")
      )|>
      hc_plotOptions(
        series = list(
          accessibility = list(enabled = FALSE),
          marker = list(
            enabled = FALSE,
            states = list(
              hover = list(enabled = FALSE)
            )
          )
        )
      )
  }

  hc
}

add_CatNumNum_features <- function(hc, data, opts, viz) {

  if (viz %in% c("bar", "column")) {
    hc <- hc |>
      hc_chart(zoomType = 'xy') |>
      hc_axis("x", categories = data$categories,
              type = "category", opts = opts) |>
      hc_tooltip(
        useHTML = TRUE,
        shared = TRUE,
        formatter = JS("function () {return this.points[0].point.label;}")
      ) |>
      # hc_tooltip(useHTML = TRUE, shared = TRUE) |>
      hc_data_series(data$data)
  }

  if (viz == "bar_line") {
    names <- names(data)
    colors <- unique(data$..colors)
    hc <- hc |>
      hc_xAxis(categories = unique(data[[1]]))|>
      hc_add_series(name = names[2], data = data[[2]],
                    type = "column", color = colors[1]) |>
      hc_add_series(name = names[3], data = data[[3]],
                    type = "spline", yAxis = 1, color = colors[2])

  }

  if (viz %in% c("bar", "bar_line", "column")) {
    hc <-  hc |>
      hc_yAxis_multiples(
        list(title = list(text = opts$title_axis_y)),
        list(title = list(text = opts$title_axis_y2),
             opposite = TRUE)
      )
  }

  if (viz == "scatter") {
    hc <- hc |>
      hc_axis(axis = "x", opts = opts) |>
      hc_axis(axis = "y", opts = opts) |>
      hc_data_series(data) |>
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS("function () {return this.point.label;}")
      ) |>
      hc_add_legend(opts = opts)
  }

  if (viz == "dumbbell") {

    hc <- hc |>
      hc_tooltip(useHTML = TRUE,
                 formatter = JS("function(){ return this.point.label; }"))|>
      hc_legend(enabled =  FALSE) |>
      hc_xAxis(type = "category", title = list(text = opts$title_axis_x)) |>
      hc_yAxis(title = list(text = opts$title_axis_y)) |>
      hc_add_series(
        data = data,
        keys = c("name", "low", "high"),
        colorByPoint = TRUE,
        colors = data$colors
      )
  }

  hc
}

add_CatNumNumNum_features <- function(hc, data, opts, viz) {

  if (viz == "scatter") {
    hc <- hc |>
      hc_axis(axis = "x", opts = opts) |>
      hc_axis(axis = "y", opts = opts) |>
      hc_data_series(data) |>
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS("function () {return this.point.label;}")
      ) |>
      hc_add_legend(opts = opts)
  }
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

  if (viz %in% c("bar", "line")) {
    hc <- hc |>
      hc_chart(zoomType = 'xy') |>
      hc_axis("x", categories = data$categories,
              type = "datetime", opts = opts) |>
      hc_data_series(data$data)
  }
  if (viz == "bar_line") {
    names <- names(data)
    colors <- unique(data$..colors)
    hc <- hc |>
      hc_xAxis(categories = unique(data[[1]]),
               type = "datetime") |>
      hc_add_series(name = names[2], data = data[[2]],
                    type = "column", color = colors[1]) |>
      hc_add_series(name = names[3], data = data[[3]],
                    type = "spline", yAxis = 1, color = colors[2])

  }

  hc <- hc |>
    hc_yAxis_multiples(
      list(title = list(text = opts$title_axis_y)),
      list(title = list(text = opts$title_axis_y2),
           opposite = TRUE)
    ) |>
    hc_tooltip(useHTML = TRUE, shared = TRUE)

  hc
}

add_CatImgNum_features <- function(hc, data, opts, viz) {
  if (viz == "bar_icons") {
    hc <- hc |>
      hc_add_dependency("modules/pattern-fill.js") |>
      hc_chart(type = "column") |>
      hc_axis(
        axis = "x", categories = data$categories,
        type = "category", opts = opts
      ) |>
      hc_axis(axis = "y", opts = opts) |>
      hc_xAxis(gridLineColor = "transparent") |>
      hc_yAxis(gridLineColor = "transparent") |>
      hc_legend(enabled = FALSE) |>
      hc_data_series(data$data)
  }
}

calculate_opacity <- function(value, min_value, max_value) {
  0.2 + 0.8 * (value - min_value) / (max_value - min_value)
}
