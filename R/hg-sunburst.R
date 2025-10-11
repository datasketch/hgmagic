#' @export
hg_sunburst <- function(data,
                        dic = NULL,
                        var_cat = NULL,
                        var_yea = NULL,
                        var_num = NULL, ...) {
  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num %||% 'count')

  ht <- hdtable(data, dic)
  var_cat <- c(var_cat, var_yea)
  data_viz <- data_processing(ht$data,
                        ht$dic,
                        var_cat,
                        var_num,
                        ...)

  color_by <- if (length(var_cat) > 1) var_cat[1] else NULL
  data_viz <- colors_data(data_viz, color_by = color_by, ...)
  data_viz <- hg_list(data_viz, hdtype, "sunburst")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_sunburst(data_viz, "CatCatNum", ...) |>
    hc_add_exporting(...)
}

#' @export
hg_sunburst_CatCat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_sunburst(data, dic, var_cat = c(vars[1], vars[2]), ...)
}

#' @export
hg_sunburst_CatCatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_sunburst(data, dic, var_cat = c(vars[1], vars[2]), var_num = vars[3], ...)
}
