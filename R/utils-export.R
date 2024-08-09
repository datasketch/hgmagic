
hc_add_exporting <- function(hc, ...) {
  opts_export <- dsopts_merge(..., categories = "export")
  if (!opts_export$export) return(hc)
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$downloadJPEG <- "Descarga JPEG"
  hcoptslang$downloadPDF <- "Descarga PDF"
  hcoptslang$downloadSVG <- "Descarga SVG"
  hcoptslang$downloadPNG <- "Descarga PNG"
  hcoptslang$viewFullscreen <- "Ver"
  hcoptslang$exitFullscreen <- "Salir"
  options(highcharter.lang = hcoptslang)

  hc$x$hc_opts$chart$events$load <- JS("
    Highcharts.SVGRenderer.prototype.symbols.download = function (x, y, w, h) {
      const path = [
        'M', x + w * 0.5, y,
        'L', x + w * 0.5, y + h * 0.7,
        'M', x + w * 0.3, y + h * 0.5,
        'L', x + w * 0.5, y + h * 0.7,
        'L', x + w * 0.7, y + h * 0.5,
        'M', x, y + h * 0.9,
        'L', x, y + h,
        'L', x + w, y + h,
        'L', x + w, y + h * 0.9
      ];
      return path;
    };
  ")
  hc |>
  hc_exporting(
    enabled = TRUE,
    buttons = list(
      contextButton = list(
        symbol = "download",
        menuItems = c(
          "downloadSVG",
          "downloadPDF",
          "downloadJPEG",
          "downloadPNG",
          "viewFullscreen"
        )
      )
    )
  )
}

