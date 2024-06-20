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

  ht <- hdtable(data, dic)
  var_cat <- c(var_cat, var_yea, var_dat)

  data_viz <- data_prep(ht$data, ht$dic, var_cat, var_num, ...)

  if (!is.null(var_dat)) data_viz <- data_viz[!is.na(data_viz[[var_dat]]), ]

  color_by <- NULL
  if (length(var_cat) > 1) {
    color_by <- var_cat[1]
    data_viz <- completevalues(data_viz, var_find = var_cat[1],
                               var_expand = var_cat[2], var_num = var_num)
  }

  if (length(var_num) > 1) color_by <- var_cat[1]
  print(data_viz)
  data_viz <- colors_data(data_viz, color_by = color_by, ...)
  print("con colores")
  print(data_viz)
  data_viz <- hg_list(data_viz, hdtype, "line")

  highchart() |>
    hc_titles(opts = dsopts_merge(..., categories = "titles")) |>
    hc_add_line(data_viz, hdtype, ...)
}

#' @export
hg_line_Dat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_line(data, dic, var_dat = vars[1], ...)
}

#' @export
hg_line_DatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_line(data, dic, var_dat = vars[1], var_num = vars[2], ...)
}


#' @export
hg_line_CatDat <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_line(data, dic, var_cat = vars[1], var_dat = vars[2], ...)
}

#' @export
hg_line_CatDatNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_line(data, dic, var_cat = vars[1], var_dat = vars[2], var_num = vars[3], ...)
}

#' @export
hg_line_DatNumNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_line(data, dic, var_dat = vars[1], var_num = c(vars[2], vars[3]), ...)
}

#' @export
hg_line_Num <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_line(data, dic, var_num = vars[1], ...)
}

#' @export
hg_line_NumNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_line(data, dic, var_num = c(vars[1], vars[2]), ...)
}

#' @export
hg_line_CatNumNum <- function(data, dic = NULL, ...) {
  vars <- data_vars(data)
  hg_line(data, dic, var_cat = vars[1], var_num = c(vars[2], vars[3]), ...)
}
