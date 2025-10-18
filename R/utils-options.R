#' @export
hc_add_options <- function(hc, viz, opts = NULL) {
  common_options <- common_options(opts)
  default_options <- default_options(viz, opts)
  final_options <- modifyList(common_options, default_options)

  args_list <- list(hc)
  args_list[[viz]] <- final_options
  do.call("hc_plotOptions", args_list)
}

default_options <- function(viz, opts) {
  stacking <- NULL

  if (viz %in% c("bar", "column")) {
    if (opts$bar_graph_type == "stacked") {
      stacking <- if (opts$percentage) "percent" else "normal"
    }
  }

  donut_inner_size <- NULL
  if (!is.null(opts$donut_inner_size)) {
    donut_inner_size <- paste0(opts$donut_inner_size, "%")
  }

  options <- list(
    bar = list(
      stacking = stacking
    ),
    column = list(
      stacking = stacking
    ),
    pie = list(
      showInLegend = opts$legend_show,
      innerSize = donut_inner_size,
      dataLabels = list(
        distance = "-40%",
        format = opts$datalabel_template %||% "{point.percentage:.1f}%"
      )
    ),
    sunburst = list(
      showInLegend = opts$legend_show,
      innerSize = donut_inner_size
    ),
    line = list(
      color = opts$palette_colors,
      marker = list(
        fillColor = "white",
        lineWidth = 2,
        lineColor = NULL
      )
    ),
    packedbubble = list(
      minSize= opts$bubble_min %||% '30%',
      maxSize = opts$bubble_max %||% '150%',
      zMin = 0,
      zMax = 1000,
      layoutAlgorithm = list(
        splitSeries = opts$bubble_cluster,
        gravitationalConstant= 0.02
      ),
      marker= list(
        fillOpacity = opts$bubble_opacity)),
    treemap = list(
      layoutAlgorithm  = opts$treemap_layout,
      # #allowDrillToNode = TRUE,
      # # animationLimit = 1000,
      # # alternateStartingDirection  = TRUE,
      borderColor  = '#fff',
      borderRadius  = 6,
      borderWidth  = 2,
      # accessibility = list(
      #   exposeAsGroupOnly = TRUE
      # ),
      # dataLabels  = list(
      #   style  = list(
      #     textOutline  = 'none'
      #   )
      # ),
      levels  = list(
        list(
          level  = 1,
          #layoutAlgorithm = opts$treemap_layout,
          dataLabels  = list(
            enabled  = TRUE,
            align  = 'left',
            verticalAlign  = 'top',
            style  = list(
              fontSize  = '15px',
              fontWeight  = 'bold'
            )
          )
        )
      )
    ),
    networkgraph = list(
      dataLabels = list(linkFormat = ""),
      layoutAlgorithm = list(enableSimulation = TRUE),
      marker = list(radius = 10)
    )
  )

  options[[viz]]
}

common_options <- function(opts) {
  list(
    enableMouseTracking = TRUE,
    dataLabels = list(
      enabled = opts$datalabel_show,
      format = opts$datalabel_template
    )
  )
}
