#' @export
hc_axis <- function(hc, axis = "x", categories = NULL, type = NULL, opts) {
  if (!axis %in% c("x", "y")) {
    stop("axis must be 'x' or 'y'")
  }

  axis_function <- if (axis == "x") hc_xAxis else hc_yAxis
  axis_title <- if (axis == "x") opts$title_axis_x else opts$title_axis_y


  hc |>
    axis_function(
      alignTicks = TRUE,
      allowDecimals = NULL,
      alternateGridColor= NULL,
      angle = 0,
      breaks = list(
        breakSize = 0,
        from= NULL,
        #repeat = 0,
        to= NULL
      ),
      categories = categories,
      ceiling= NULL,
      className= NULL,
      crosshair = list(
        className= NULL,
        color = "#cccccc",
        dashStyle = "Solid",
        snap = TRUE,
        width = 1,
        zIndex = 2
      ),
      crossing = NULL,
      # dateTimeLabelFormats = list(
      #   day = list(
      #     list = NULL,
      #     main = "%e %b"
      #   )
      # )
      endOnTick = TRUE,
      # events = list(
      #   afterBreaks = NULL
      #   afterSetExtremes = NULL
      #   pointBreak = NULL
      #   pointBreakOut = NULL
      #   pointInBreak = NULL
      #   setExtremes = NULL
      # )
      floor = NULL,
      gridLineColor = "#e6e6e6",
      gridLineDashStyle = "Solid",
      gridLineInterpolation = NULL,
      gridLineWidth = 1,
      gridZIndex = 1,
      height = NULL,
      id = NULL,
      labels = list(
        align = NULL,
        allowOverlap = FALSE,
        autoRotation = NULL,
        autoRotationLimit = 80,
        distance = 15,
        enabled = TRUE,
        format = NULL,
        formatter = NULL,
        maxStaggerLines = 5,
        overflow = "justify",
        padding = 5,
        position3d = "offset",
        reserveSpace = NULL,
        rotation = 0,
        skew3d = FALSE,
        staggerLines = 0,
        step = 0,
        # style = list(...)
        useHTML = TRUE,
        x = NULL, #separacion de etiquetas respecto al eje x
        y = NULL, #separacion de etiquetas respecto al eje y
        zIndex = 7
      ),
      left = NULL,
      lineColor = "#333333",
      lineWidth = 0,
      linkedTo = NULL,
      margin = NULL,
      max = NULL,
      maxColor = "#003399",
      maxPadding = 0.05,
      maxZoom = NULL,
      min = NULL,
      minColor = "#e6ebf5",
      minorGridLineColor = "#f2f2f2",
      minorGridLineDashStyle = "Solid",
      minorGridLineWidth = 1,
      minorTickColor = "#999999",
      minorTickInterval = NULL,
      minorTickLength = 2,
      minorTickPosition = "outside",
      minorTicks = FALSE,
      minorTicksPerMajor = 5,
      minorTickWidth = 0,
      minPadding = 0.05,
      minRange = NULL,
      minTickInterval = NULL,
      offset = NULL,
      opposite = FALSE,
      pane = NULL,
      panningEnabled = TRUE,
      # plotBands = [list(
      #   borderColor = NULL,
      #   borderWidth = 0
      #   className = NULL,
      #   color = #e6e9ff
      #     events = list(
      #       click = NULL,
      #       mousemove = NULL,
      #       mouseout = NULL,
      #       mouseover = NULL,
      #     )
      #   from = NULL,
      #   id = NULL,
      #   innerRadius = NULL,
      #   label = list(
      #     align = center
      #     rotation = 0
      #     style = NULL,
      #     text = NULL,
      #     textAlign = NULL,
      #     useHTML = FALSE,
      #     verticalAlign = top
      #     x = NULL,
      #     y = NULL,
      #   )
      #   outerRadius = 100%
      #   thickness = 10
      #   to = NULL,
      #   zIndex = NULL,
      # )]
      # plotLines = [list(
      #   className = NULL,
      #   color = #999999
      #     dashStyle = Solid
      #   events = list(
      #     click = NULL,
      #     mousemove = NULL,
      #     mouseout = NULL,
      #     mouseover = NULL,
      #   )
      #   id = NULL,
      #   label = list(
      #     align = left
      #     formatter = NULL,
      #     rotation = NULL,
      #     style = NULL,
      #     text = NULL,
      #     textAlign = NULL,
      #     useHTML = FALSE,
      #     verticalAlign = top
      #     x = NULL,
      #     y = NULL,
      #   )
      #   labels = list(
      #     clip = FALSE,
      #   )
      #   value = NULL,
      #   width = 2
      #   zIndex = NULL,
      # )]
      # reversed = FALSE,
      # reversedStacks = TRUE,
      # showEmpty = TRUE,
      # showFirstLabel = TRUE,
      # showLastLabel = NULL,
      # softMax = NULL,
      # softMin = NULL,
      # stackLabels = list(
      #   align = NULL,
      #   allowOverlap = FALSE,
      #   animation = list(
      #     defer = NULL,
      #   )
      #   backgroundColor = NULL,
      #   borderColor = NULL,
      #   borderRadius = 0
      #   borderWidth = 0
      #   crop = TRUE,
      #   enabled = FALSE,
      #   format = list(total)
      #   formatter = NULL,
      #   overflow = justify
      #   rotation = 0
      #   style = list(
      #     color = #000000
      #       fontSize = 0.7em
      #     fontWeight = bold
      #     textOutline = 1px contrast
      #   )
      #   textAlign = NULL,
      #   useHTML = FALSE,
      #   verticalAlign = NULL,
      #   x = NULL,
      #   y = NULL,
      # )
      stackShadow = list(
        borderColor = "transparent",
        borderWidth = 0,
        color = "#dedede",
        enabled = NULL
      ),
      startOfWeek = 1,
      startOnTick = TRUE,
      stops = NULL,
      tickAmount = NULL,
      tickColor = "#333333",
      tickInterval = NULL,
      tickLength = 10,
      tickmarkPlacement = "between",
      tickPixelInterval = 72,
      tickPosition = "outside",
      tickPositioner = NULL,
      tickPositions = NULL,
      tickWidth = 0,
      title = list(
        align = "middle",
        enabled = NULL,
        margin = 40,
        offset = NULL,
        position3d = NULL,
        reserveSpace = TRUE,
        rotation = NULL,
        skew3d = NULL,
        style = list(
          color = "#666666",
          fontSize = "0.8em"
        ),
        text = axis_title,
        textAlign = NULL,
        useHTML = FALSE,
        x = 0,
        y = 0
      ),
      tooltipValueFormat = NULL,
      top = NULL,
      type = type,
      uniqueNames = TRUE,
      units = NULL,
      visible = TRUE,
      width = NULL,
      zIndex = 2,
      zoomEnabled = TRUE
    )

}


