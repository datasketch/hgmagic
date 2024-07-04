#' @export
hc_axis <- function(hc, axis = "x", categories = NULL, type = NULL, opts) {
  if (!axis %in% c("x", "y")) {
    stop("axis must be 'x' or 'y'")
  }

  axis_function <- if (axis == "x") hc_xAxis else hc_yAxis
  axis_title <- if (axis == "x") opts$title_axis_x else opts$title_axis_y
  axis_labels <- NULL
  if (axis == "y") {
    axis_labels <-  paste0(opts$prefix, "{value}", opts$suffix)
  }

  hc |>
    axis_function(
      categories = categories,
      crossing = NULL,
      endOnTick = TRUE,
      labels = list(
        format = axis_labels
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


