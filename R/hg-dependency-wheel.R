#' @export
hg_dependency_wheel <- function(data,
                                dic = NULL,
                                var_cat = NULL,
                                var_yea = NULL,
                                var_num = NULL, ...) {

  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num %||% 'count')

  var_cat <- c(var_cat, var_yea)
  data_viz <- data_processing(na.omit(data),
                        dic,
                        var_cat,
                        var_num,
                        text_wrap = 500,
                        legend_text_wrap = 500,
                        axis_text_wrap = 500,
                        ...)

  data_viz <- hg_list(data_viz, hdtype, "dependency_wheel")

  data_viz$nodes <- colors_data(data_viz$nodes, color_by = "id", ...) |>
    rename(color = ..colors)

  data_viz$data$color <- data_viz$nodes$color[
    match(data_viz$data$from, data_viz$nodes$id)
  ]

  data_viz$data$color <- sub("FF$", "", data_viz$data$color)
  data_viz$nodes <- list_parse(data_viz$nodes)

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_dependency_wheel(data_viz, hdtype, ...) |>
    hc_add_exporting(...)
}

#' @export
hg_dependency_wheel_CatCat <- function(data,
                                       dic = NULL,
                                       ...) {
  vars <- data_vars(data)
  hg_dependency_wheel(data,
                      dic,
                      var_cat = c(vars[1], vars[2]),
                      ...)
}

#' @export
hg_dependency_wheel_CatCatNum <- function(data,
                                          dic = NULL,
                                          ...) {
  vars <- data_vars(data)
  hg_dependency_wheel(data,
                      dic,
                      var_cat = c(vars[1], vars[2]),
                      var_num = vars[3], ...)
}
