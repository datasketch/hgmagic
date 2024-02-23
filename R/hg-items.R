#' @export
hg_item <- function(data,
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

  color_by <- var_cat[1]
  data_viz <- colors_data(data_viz, color_by = color_by, ...)

  data_viz <- hg_list(data_viz, hdtype, "item")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_item(data_viz, hdtype, ...)
}
