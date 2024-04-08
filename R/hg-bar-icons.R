#' @export
hg_bar_icons <- function(data,
                       dic = NULL,
                       var_cat = NULL,
                       var_yea = NULL,
                       var_img = NULL,
                       var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_img = var_img,
                       var_num = var_num %||% "count")

  ht <- hdtable(data, dic)
  var_cat <- c(var_cat, var_yea)

  data_viz <- ht$data
  data_viz <- hg_list(data_viz, hdtype, "bar_icons")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_bar_icons(data_viz, hdtype, ...)
}

#' @export
hg_bar_icons_CatImgNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_bar_icons(
    data, dic, var_cat = vars[1],
    var_img = vars[2],
    var_num = vars[3],
    ...
  )
}
