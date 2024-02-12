#' @export
hc_titles <- function(hc, ...) {
  opts <- dsopts_merge(..., categories = "titles")
  hc |>
    hc_title(text = opts$title) |>
    hc_subtitle(text = opts$subtitle)  |>
    hc_credits(enabled = opts$caption_show, text = opts$caption)
}
