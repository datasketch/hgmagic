#' @export
hc_titles <- function(hc, ...) {
  opts <- dsopts_merge(..., categories = "titles")
  if (opts$org == "elconfidencial") {
    if (!is.null(opts$title)) {
      opts$title <- paste0(opts$title,
                                 "<hr style='width: 72px;height: 2px;border: none;background-color: #000000;position: absolute;margin-bottom:15px;'><hr style='color: transparent !important;'>")
    }
  }
  hc |>
    hc_title(text = opts$title, useHTML = TRUE) |>
    hc_subtitle(text = opts$subtitle, useHTML = TRUE)  |>
    hc_credits(enabled = opts$caption_show, text = opts$caption, useHTML = TRUE)
}
