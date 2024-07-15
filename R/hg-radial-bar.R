#' @export
hg_radial_bar <- function(data,
                          dic = NULL,
                          var_cat = NULL,
                          var_yea = NULL,
                          var_num = NULL, ...) {

  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num %||% "count")

  ht <- hdtable(data, dic)
  var_cat <- c(var_cat, var_yea)
  data_viz <- ht$data

  if (length(var_cat) > 1) {
    data_viz <- completevalues(data_viz, var_find = var_cat[1],
                               var_expand = var_cat[2], var_num = var_num)
  }

  data_viz <- data_prep(data_viz, ht$dic, var_cat, var_num, ...)
  data_viz <- colors_data(data_viz,  ...)
  data_viz <- hg_list(data_viz, hdtype, "radial_bar")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_radial_bar(data_viz, hdtype, ...)
}

#' @export
hg_radial_bar_Cat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_radial_bar(data, dic, var_cat = vars[1], ...)
}

#' @export
hg_radial_bar_CatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_radial_bar(data, dic, var_cat = vars[1], var_num = vars[2], ...)
}

#' @export
hg_radial_bar_CatCat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_radial_bar(data, dic, var_cat = c(vars[1], vars[2]), ...)
}

#' @export
hg_radial_bar_CatCatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_radial_bar(data, dic, var_cat = c(vars[1], vars[2]), var_num = vars[3], ...)
}
