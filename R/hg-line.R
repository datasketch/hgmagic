#' @export
hg_line <- function(data,
                   dic = NULL,
                   var_cat = NULL,
                   var_yea = NULL,
                   var_dat = NULL,
                   var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_dat = var_dat,
                       var_num = var_num %||% 'count')

  ht <- hdtable(data)
  var_cat <- c(var_cat, var_yea)
  data_viz <- data_prep(ht$data, ht$dic, var_cat, var_num, ...)

  color_by <- NULL
  if (length(var_cat) > 1) {
    color_by <- var_cat[1]
    data_viz <- completevalues(data_viz, var_find = var_cat[1],
                               var_expand = var_cat[2], var_num = var_num)
  }
  if (length(var_num) > 1) color_by <- var_cat
  data_viz <- colors_data(data_viz, color_by = color_by, ...)
  data_viz <- hg_list(data_viz, hdtype, "line")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_line(data_viz, hdtype, ...)
}
