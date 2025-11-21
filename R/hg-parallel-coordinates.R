#' @export
hg_parallel_coordinates <- function(data,
                                    dic = NULL,
                                    var_cat = NULL,
                                    var_yea = NULL,
                                    var_num = NULL, ...) {
  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num)


  var_cat <- c(var_cat, var_yea)
  data_viz <- data_processing(data, dic, var_cat, var_num, viz = "parallel_coordinates", ...)
  data_viz <- colors_data(data_viz, var_cat = var_cat, var_num = var_num, ...)
  data_viz <- hg_list(data_viz, hdtype, "parallel_coordinates")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_parallel_coordinates(data_viz, hdtype, ...) |>
    hc_add_exporting(...)
}

hg_parallel_coordinates_CatCatCatCatCatCatCat <- function(data,
                                                          dic = NULL,
                                                          ...) {
  vars <- data_vars(data)
  hg_parallel_coordinates(
    data, dic,
    var_cat = c(
      vars[1:7]
    ),
    ...
  )
}
