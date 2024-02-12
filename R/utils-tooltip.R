#' @export
hc_add_tooltip <- function(hc) {
  hc <- hc |>
    hc_tooltip(
      animation = TRUE,
      backgroundColor = "#ffffff",
      borderColor = NULL,
      borderRadius = 3,
      borderWidth = NULL,
      className = NULL,
      clusterFormat = "Clustered",
      points =  "{point.clusterPointsAmount}",
      crosshairs = TRUE,
      #dateTimeLabelFormats = {...}
      distance = 16,
      enabled = TRUE,
      followPointer = FALSE,
      followTouchMove = TRUE,
      footerFormat = NULL,
      format = NULL,
      formatter = NULL, #JS("function () {return this.point.label;}")
      headerFormat = NULL,
      #headerShape = callout,
      hideDelay = 500,
      nullFormat = NULL,
      nullFormatter = NULL,
      outside = NULL,
      padding = 8,
      #pointFormat = NULL,
      #pointFormatter = NULL,
      positioner = NULL,
      shadow = TRUE,
      #shape = callout
      shared = FALSE,
      #snap = 10/25
      split = FALSE,
      stickOnContact = FALSE,
      style = list(
        color = "#333333",
        # cursor = default,
        fontSize = "0.8em"
      ),
      useHTML = TRUE,
      valueDecimals = NULL,
      valuePrefix = NULL,
      valueSuffix = NULL,
      xDateFormat = NULL
    )
  hc
}
