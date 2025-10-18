#' @export
hc_axis <- function(hc, axis = "x", categories = NULL, type = NULL, opts, double_axis = FALSE) {
  if (!axis %in% c("x", "y")) {
    stop("axis must be 'x' or 'y'")
  }

  # Handle double axis functionality
  if (double_axis && axis == "y") {
    return(hc_axis_double_y(hc, opts))
  }

  #  print(opts$percentage)
  # if (!is.null(opts$percentage) && opts$percentage && opts$percentage_axis) {
  #   opts$axis_y_suffix <- opts$axis_y_suffix %||% "%"
  # }

  axis_function <- if (axis == "x") hc_xAxis else hc_yAxis
  axis_title <- if (axis == "x") opts$title_axis_x else opts$title_axis_y
  axis_labels <- NULL
  axis_format <- NULL
  if (axis == "y") {
    axis_labels <-  paste0(opts$axis_y_prefix, "{text}", opts$axis_y_suffix)
    if (!is.null(opts$axis_y_format_sample_num) || !is.null(opts$format_sample_num)) {
    axis_format <- makeup::makeup_format_js(opts$axis_y_format_sample_num, opts$locale,
                                    opts$axis_y_suffix, opts$axis_y_prefix,
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

#' Internal function to handle double y-axis
#' @noRd
hc_axis_double_y <- function(hc, opts) {
  # Get axis titles - handle both single values and vectors

  axis_title_1 <- if (is.vector(opts$title_axis_y) && length(opts$title_axis_y) >= 1) {
    opts$title_axis_y[1]
  } else {
    opts$title_axis_y
  }

  axis_title_2 <- if (is.vector(opts$title_axis_y) && length(opts$title_axis_y) >= 2) {
    opts$title_axis_y[2]
  } else if (!is.null(opts$title_axis_y2)) {
    opts$title_axis_y2
  } else {
    axis_title_1  # Use same title for both axes if only one provided
  }

  # Handle axis formatting
  axis_format_1 <- NULL
  axis_format_2 <- NULL

  if (!is.null(opts$axis_y_format_sample_num) || !is.null(opts$format_sample_num)) {
    format_sample <- opts$axis_y_format_sample_num %||% opts$format_sample_num

    # If format_sample is a vector with 2 elements, use them separately
    if (is.vector(format_sample) && length(format_sample) >= 2) {
      axis_format_1 <- makeup::makeup_format_js(format_sample[1], opts$locale,
                                                opts$axis_y_suffix, opts$axis_y_prefix,
                                                opts$use_si_prefixes)
      axis_format_2 <- makeup::makeup_format_js(format_sample[2], opts$locale,
                                                opts$axis_y_suffix, opts$axis_y_prefix,
                                                opts$use_si_prefixes)
    } else {
      # Use same format for both axes
      axis_format_1 <- makeup::makeup_format_js(format_sample, opts$locale,
                                                opts$axis_y_suffix, opts$axis_y_prefix,
                                                opts$use_si_prefixes)
      axis_format_2 <- axis_format_1
    }
  }

  # Create axis labels
  axis_labels <- paste0(opts$axis_y_prefix, "{text}", opts$axis_y_suffix)

  # Apply the double y-axis configuration
  hc |>
    hc_yAxis_multiples(
      # EJE 1
      list(
        title = list(
          text = axis_title_1,
          style = list(
            color = "#666666",
            fontSize = "0.8em"
          ),
          useHTML = TRUE
        ),
        labels = list(
          format = axis_labels,
          formatter = axis_format_1
        ),
        crossing = NULL,
        endOnTick = FALSE,
        startOnTick = TRUE
      ),
      # EJE 2
      list(
        title = list(
          text = axis_title_2,
          style = list(
            color = "#666666",
            fontSize = "0.8em"
          ),
          useHTML = TRUE
        ),
        labels = list(
          format = axis_labels,
          formatter = axis_format_2
        ),
        opposite = TRUE,
        crossing = NULL,
        endOnTick = FALSE,
        startOnTick = TRUE
      )
    )
}


