#' @export
hg_dumbbell <- function(data,
                        dic = NULL,
                        var_cat = NULL,
                        var_yea = NULL,
                        var_num = NULL, ...) {
  if (is.null(data)) stop("You must enter a dataset")
  hdtype <- hdtype_viz(var_cat = var_cat,
                       var_yea = var_yea,
                       var_num = var_num)


  var_cat <- c(var_cat, var_yea)
  data_viz <- data_processing(data,
                        dic,
                        var_cat,
                        var_num,
                        ...)

  color_by <- var_cat[1]
  data_viz <- colors_data(data_viz, color_by = color_by, ...)
  data_viz <- hg_list(data_viz, hdtype, "dumbbell")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_dumbbell(data_viz, hdtype, ...) |>
    hc_add_exporting(...)
}

#' @export
hg_dumbbell_CatNumNum <- function(data,
                             dic = NULL,
                             ...) {
  vars <- data_vars(data)
  hg_dumbbell(data,
            dic,
            var_cat = vars[1],
            var_num = c(vars[2], vars[3]),
            ...)
}
