#' @export
hg_scatter <- function(data,
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

  if (length(var_cat) == 0) data_viz <- data
  else data_viz <- data_prep(ht$data, ht$dic, var_cat, var_num, ...)

  color_by <- NULL
  if (length(var_cat) > 1) {
    color_by <- var_cat[1]

    data_viz <- completevalues(data_viz, var_find = var_cat[1],
                               var_expand = var_cat[2], var_num = var_num)
  }

  data_viz <- colors_data(data_viz, color_by = color_by, ...)

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_scatter(data_viz, hdtype, ...)
}

#' @export
hg_scatter_NumNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_scatter(data, dic, var_num = c(vars[1], vars[2]), ...)
}

#' @export
hg_scatter_CatNumNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_scatter(data, dic, var_cat = vars[1], var_num = c(vars[2], vars[3]), ...)
}

#' @export
hg_scatter_CatCatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_scatter(data, dic, var_cat = c(vars[1], vars[2]), var_num = vars[3], ...)
}
