#' @export
hc_axis <- function(hc, axis = "x", categories = NULL, type = NULL, opts) {
  if (!axis %in% c("x", "y")) {
    stop("axis must be 'x' or 'y'")
  }

  axis_function <- if (axis == "x") hc_xAxis else hc_yAxis
  axis_title <- if (axis == "x") opts$title_axis_x else opts$title_axis_y
  axis_labels <- NULL
  axis_format <- NULL
  if (axis == "y") {
    axis_labels <-  paste0(opts$axis_y_prefix, "{text}", opts$axis_y_suffix)
    if (!is.null(opts$axis_y_format_sample_num)) {
    axis_format <- makeup::makeup_format_js(opts$axis_y_format_sample_num, opts$locale,
                                    opts$axis_y_prefix, opts$axis_y_suffix,
                                    opts$use_si_prefixes)
    }
  }

  hc |>
    axis_function(
      categories = categories,
      crossing = NULL,
      endOnTick = FALSE,
      startOnTick = TRUE,
      labels = list(
        format = axis_labels,
        formatter = axis_format
      ),
      title = list(
        style = list(
          color = "#666666",
          fontSize = "0.8em"
        ),
        text = axis_title,
        # textAlign = NULL,
        useHTML = TRUE#,
        # x = 0,
        # y = 0
      )
    )

}


