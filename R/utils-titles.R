#' @export
hc_titles <- function(hc, ...) {
  opts <- dsopts_merge(..., categories = "titles")
  hc |>
    hc_title(text = opts$title, useHTML = TRUE) |>
    hc_subtitle(text = opts$subtitle, useHTML = TRUE)  |>
    hc_credits(enabled = opts$caption_show, text = opts$caption, useHTML = TRUE)
}
