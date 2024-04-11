#' @export
hc_add_options <- function(hc, viz, opts = NULL) {
  default_options <- default_options(viz, opts)
  common_options <- common_options(opts)
  final_options <- modifyList(default_options, common_options)
  args_list <- list(hc)
  args_list[[viz]] <- final_options
  hc <- do.call("hc_plotOptions", args_list)
  hc
}



default_options <- function(viz, opts) {

  stacking <- NULL
  if (viz %in% c("bar", "column")) {
    if (opts$bar_graph_type == "stacked") {
      stacking <- "normal"
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
    treemap = list(
      layoutAlgorithm  = 'sliceAndDice',
      allowDrillToNode = TRUE,
      animationLimit = 1000,
      alternateStartingDirection  = TRUE,
      borderColor  = '#fff',
      borderRadius  = 6,
      borderWidth  = 2,
      accessibility = list(
        exposeAsGroupOnly = TRUE
      ),
      dataLabels  = list(
        style  = list(
          textOutline  = 'none'
        )
      ),
      levels  = list(
        list(
          level  = 1,
          layoutAlgorithm  = 'sliceAndDice',
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
    )
  )

  options[[viz]]
}


common_options <- function(opts) {
  list(
    enableMouseTracking = TRUE,
    dataLabels = list(enabled = opts$datalabel_show)
  )
}

