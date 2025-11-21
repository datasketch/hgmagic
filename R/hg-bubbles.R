#' @export
hg_bubbles <- function(data,
                       dic = NULL,
                       var_cat = NULL,
                       var_yea = NULL,
                       var_num = NULL, ...) {

  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num %||% 'count')

  opts <- list(...)
  var_cat <- c(var_cat, var_yea)


  color_by <- if (length(var_cat) > 1) var_cat[1] else opts$color_by
  data_viz <- data_processing(data, dic, var_cat, var_num, ...)
  data_viz <- colors_data(data_viz, color_by = color_by, ...)

  data_viz <- hg_list(data_viz, hdtype, "bubble")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_bubbles(data_viz, hdtype, ...) |>
    hc_add_exporting(...)


}


#' @export
hg_bubbles_Cat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_bubbles(data, dic, var_cat = vars[1], ...)
}

#' @export
hg_bubbles_CatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_bubbles(data, dic, var_cat = vars[1], var_num = vars[2], ...)
}

#' @export
hg_bubbles_CatCat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_bubbles(data, dic, var_cat = c(vars[1], vars[2]), ...)
}

#' @export
hg_bubbles_CatCatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_bubbles(data, dic, var_cat = c(vars[1], vars[2]), var_num = vars[3], ...)
}
