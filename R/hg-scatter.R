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
  opts <- dsopts_merge(..., categories = "scatter")
  data_viz <- ht$data

  if (length(var_cat) > 1) {
    data_viz <- completevalues(data_viz, var_find = var_cat[1],
                               var_expand = var_cat[2], var_num = var_num)
  }

  if (length(var_cat) == 1) color_by <- var_cat[1]
  else if (length(var_cat) == 2) color_by <- var_cat[2]
  else color_by <- NULL

  data_viz <- data_processing(data_viz, ht$dic, var_cat, var_num, ...)
  data_viz <- colors_data(data_viz, color_by = color_by, ...)
  data_viz <- data_viz |>
    select(c(var_cat, var_num, ends_with("labels"), ends_with("colors"))) |>
    hg_list(hdtype, "scatter")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_scatter(data_viz, hdtype, ...) |>
    hc_add_exporting(...)
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
hg_scatter_CatNumNumNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_scatter(
    data, dic, var_cat = vars[1],
    var_num = c(vars[2], vars[3], vars[4]),
    ...
  )
}

#' @export
hg_dots_CatCatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_scatter(data, dic, var_cat = c(vars[1], vars[2]), var_num = vars[3], ...)
}
