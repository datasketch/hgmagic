#' @export
hc_add_legend <- function(hc, opts) {
print(opts$legend_show)
  hc |>
    hc_legend(
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(
          enabled = TRUE
        )
      ),
      align = opts$legend_align,
      alignColumns = TRUE,
      backgroundColor = NULL,
      borderColor = "#999999",
      borderRadius = 0,
      borderWidth = 0,
      bubbleLegend = list(
        borderColor = NULL,
        borderWidth = 2,
        className = NULL,
        color = NULL,
        connectorClassName = NULL,
        connectorColor = NULL,
        connectorDistance = 60,
        connectorWidth = 1,
        enabled = FALSE,
        labels = list(
          align = "right",
          allowOverlap = FALSE,
          className = NULL,
          format = NULL,
          formatter = NULL,
          style = NULL,
          x = 0,
          y = 0
        ),
        legendIndex = 0,
        maxSize = 60,
        minSize = 10#,
        # ranges = list(
        #   list(
        #     borderColor = "#DDDDDD",
        #     color = "#DDDDDD"#,
        #     #connectorColor = NULL#,
        #     #value = NULL
        #   ),
        #   sizeBy = "area",
        #   sizeByAbsoluteValue = FALSE,
        #   zIndex = 1,
        #   zThreshold = 0
        # )
      ),
        #className = "highcharts-no-tooltip",
        enabled = opts$legend_show,
        floating = FALSE,
        # itemCheckboxStyle = list("width" =  "13px",
        #                          "height" =  "13px",
        #                          "position" = "absolute"),
        # itemDistance = 20,
        # itemHiddenStyle = list("color" =  "#cccccc"),
        # itemHoverStyle = list("color" =  "#000000"),
        # itemMarginBottom = 2,
        # itemMarginTop = 2,
        # itemStyle = list("color" =  "#333333",
        #                  "cursor" =  "pointer",
        #                  "fontSize" =  "0.75em",
        #                  "fontWeight" =  "bold",
        #                  "textOverflow" =  "ellipsis"),
        # itemWidth = NULL,
        # #labelFormat = list(name),
        # labelFormatter = NULL,
        layout = opts$legend_orientation, #horizontal, vertical or proximate
        lineHeight = 16,
        margin = 12,
        maxHeight = NULL,
        navigation = list(
          activeColor = "#0022ff",
          animation = TRUE,
          arrowSize = 12,
          enabled = TRUE,
          inactiveColor = "#cccccc",
          style = NULL
        ),
        padding = 8,
        reversed = FALSE,
        rtl = FALSE,
        shadow = FALSE,
        squareSymbol = TRUE,
        style = NULL,
        symbolHeight = NULL,
        symbolPadding = 5,
        symbolRadius = NULL,
        symbolWidth = NULL,
        title = list(
          style = list("fontSize" =  "0.75em", "fontWeight" =  "bold"),
          text = NULL
        ),
        useHTML = TRUE,
        valueDecimals = -1,
        valueSuffix = '',
        verticalAlign = opts$legend_vertical_align, #top, middle or bottom
        width = NULL,
        x = 0,
        y = 0
    )
}
