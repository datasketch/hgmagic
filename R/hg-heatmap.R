#' @export
hg_heatmap <- function(data,
                       dic = NULL,
                       var_cat = NULL,
                       var_yea = NULL,
                       var_num = NULL, ...) {


  if (is.null(data)) stop("You must enter a dataset")
  opts_color <- dsopts_merge(..., categories = "colorprep")

  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num %||% "count")

  ht <- hdtable(data, dic)
  var_cat <- c(var_cat, var_yea)
  data_viz <- data_processing(ht$data, ht$dic, var_cat, var_num, ...)
  data_viz <- hg_list(data_viz, hdtype, "heatmap")

  palette <- opts_color$color_palette_sequential %||% c("#BFFFFF", "#C200F8")
  data_viz$color <- c(palette[1], palette[length(palette)])

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_heatmap(data_viz, hdtype, ...) |>
    hc_add_exporting(...)
}

#' @export
hg_heatmap_CatCat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_heatmap(data, dic, var_cat = c(vars[1], vars[2]), ...)
}

#' @export
hg_heatmap_CatCatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_heatmap(data, dic, var_cat = c(vars[1], vars[2]), var_num = vars[3], ...)
}
