#' @export
hg_network_graph <- function(data,
                             dic = NULL,
                             var_cat = NULL,
                             var_yea = NULL,
                             var_num = NULL, ...) {

  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num %||% "count")

  var_cat <- c(var_cat, var_yea)
  data_viz <- data_processing(data, dic, var_cat, var_num, viz = "bar", ...)
  data_viz <- hg_list(data_viz, hdtype, "network_graph")

  highchartzero() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_network_graph(data_viz, hdtype, ...) |>
    hc_add_exporting(...)


}


#' @export
hg_network_graph_CatCat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_network_graph(data, dic, var_cat = c(vars[1], vars[2]), ...)
}
