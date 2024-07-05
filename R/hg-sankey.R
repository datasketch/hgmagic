#' @export
hg_sankey <- function(data,
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
  data_viz <- data_prep(na.omit(ht$data),
                        ht$dic,
                        var_cat,
                        var_num,
                        text_wrap = 500,
                        legend_text_wrap = 500,
                        axis_text_wrap = 500,
                        ...)

  data_viz <- hg_list(data_viz, hdtype, "sankey")

  data_viz$nodes <- colors_data(data_viz$nodes, color_by = "id", ...) |>
    rename(color = ..colors)

  data_viz$data$color <- data_viz$nodes$color[
    match(data_viz$data$from, data_viz$nodes$id)
  ]

  data_viz$data$color <- sub("FF$", "80", data_viz$data$color)
  data_viz$nodes <- list_parse(data_viz$nodes)

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_sankey(data_viz, hdtype, ...)
}

#' @export
hg_sankey_CatCat <- function(data,
                             dic = NULL,
                             ...) {
  vars <- data_vars(data)
  hg_sankey(data,
            dic,
            var_cat = c(vars[1], vars[2]),
            ...)
}

#' @export
hg_sankey_CatCatNum <- function(data,
                                dic = NULL,
                                ...) {
  vars <- data_vars(data)
  hg_sankey(data,
            dic,
            var_cat = c(vars[1], vars[2]),
            var_num = vars[3], ...)
}

#' @export
hg_sankey_CatCatCat <- function(data,
                                dic = NULL,
                                ...) {
  vars <- data_vars(data)
  hg_sankey(data,
            dic,
            var_cat = c(vars[1], vars[2], vars[3]),
            ...)
}

#' @export
hg_sankey_CatCatCatNum <- function(data,
                                   dic = NULL,
                                   ...) {
  vars <- data_vars(data)
  hg_sankey(data,
            dic,
            var_cat = c(vars[1], vars[2], vars[3]),
            var_num = vars[4], ...)
}
