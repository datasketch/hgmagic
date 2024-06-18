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

  ht <- hdtable(data, dic)
  var_cat <- c(var_cat, var_yea)
  data_viz <- data_prep(ht$data,
                        ht$dic,
                        var_cat,
                        var_num,
                        text_wrap = 500,
                        legend_text_wrap =500,
                        axis_text_wrap = 500,
                        ...)

  color_by <- if (length(var_cat) > 1) var_cat[1] else NULL

  data_viz <- colors_data(data_viz, color_by = color_by, ...)
  data_viz <- hg_list(data_viz, hdtype, "parallel_coordinates")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_parallel_coordinates(data_viz, hdtype, ...)
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
