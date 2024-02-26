#' @export
hg_treemap <- function(data,
                   dic = NULL,
                   var_cat = NULL,
                   var_yea = NULL,
                   var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num %||% 'count')

  ht <- hdtable(data)
  var_cat <- c(var_cat, var_yea)
  data_viz <- data_prep(ht$data, ht$dic, var_cat, var_num, ...)

  color_by <- var_cat
  if (length(var_cat) > 1) {
    color_by <- var_cat[1]
    data_viz <- completevalues(data_viz, var_find = var_cat[1],
                               var_expand = var_cat[2], var_num = var_num)
  }
  if (length(var_num) > 1) color_by <- var_cat
  data_viz <- colors_data(data_viz, color_by = color_by, ...)
  data_viz <- hg_list(data_viz, hdtype, "treemap")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_treemap(data_viz, hdtype, ...)
}


#' @export
hg_treemap_Cat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_treemap(data, dic, var_cat = vars[1], ...)
}

#' @export
hg_treemap_CatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_treemap(data, dic, var_cat = vars[1], var_num = vars[2], ...)
}


#' @export
hg_treemap_CatCat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_treemap(data, dic, var_cat = c(vars[1], vars[2]), ...)
}

#' @export
hg_treemap_CatCatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_treemap(data, dic, var_cat = c(vars[1], vars[2]), var_num = vars[3], ...)
}

#' @export
hg_treemap_CatNumNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_treemap(data, dic, var_cat = vars[1], var_num = c(vars[2], vars[3]), ...)
}
