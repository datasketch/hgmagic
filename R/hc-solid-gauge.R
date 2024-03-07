# data is a number between zero and one
#' @export
hg_solidgauge <- function(data,
                          dic = NULL,
                          var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")

  data_viz <- data


  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_solid_gauge(data_viz, "Num", ...)
}
