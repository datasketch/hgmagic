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

  options <- list(
    bar = list(
      stacking = stacking
    ),
    column = list(
      stacking = stacking
    ),
    pie = list(
      showInLegend = opts$legend_show
    ),
    line = list(
      color = "blue",
      marker = list(
        fillColor = "white",
        lineWidth = 2,
        lineColor = NULL
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

