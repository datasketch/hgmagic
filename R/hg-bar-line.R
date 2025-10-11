#' @export
hg_bar_line <- function(data,
                        dic = NULL,
                        var_cat = NULL,
                        var_dat = NULL,
                        var_yea = NULL,
                        var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_dat = var_dat,
                       var_yea = var_yea,
                       var_num = var_num %||% 'count')

  ht <- hdtable(data, dic)
  var_cat <- c(var_cat, var_yea, var_dat)
  data_viz <- data_processing(ht$data, ht$dic, var_cat, var_num, ...)

  color_by <- NULL
  if (length(var_cat) > 1) {
    color_by <- var_cat[1]
    data_viz <- completevalues(data_viz, var_find = var_cat[1],
                               var_expand = var_cat[2], var_num = var_num)
  }
  if (length(var_num) > 1) color_by <- var_cat
  data_viz <- colors_data(data_viz, color_by = color_by, ...)


  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_bar_line(data_viz, hdtype, ...) |>
    hc_add_exporting(...)
}



#' @export
hg_bar_line_CatNumNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_bar_line(data, dic, var_cat = vars[1], var_num = c(vars[2], vars[3]), ...)
}

hg_bar_line_DatNumNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_bar_line(data, dic, var_dat = vars[1], var_num = c(vars[2], vars[3]), ...)
}

