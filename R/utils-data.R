data_processing <- function(data,
                            dic = NULL,
                            var_group = NULL,
                            var_num = NULL,
                            var_dat = NULL,
                            viz = NULL,
                            ...) {

  data_agg <- aggregate_data(data = data,
                             dic = dic,
                             group_vars = var_group,
                             var_num_to_agg = var_num, ...)

  # Handle both cases: aggregate_data may return a list or a data frame directly
  if (is.list(data_agg) && !is.data.frame(data_agg)) {
    # Old behavior: returns list with $data and $dic
    agg_data <- data_agg$data
    agg_dic <- data_agg$dic
  } else {
    # New behavior: returns data frame directly
    agg_data <- data_agg
    agg_dic <- dic
  }

  data <- wrap_sort_data(data = agg_data,
                         var_cat_order = var_group,
                         var_num_sort = var_num, viz = viz, ...)
  if (!is.null(viz)) {
    if (viz == "line") {
      if (!is.null(var_dat)) {
        data <- convert_dates(data, var_dat = var_dat)
      }
    }
  }

  data
}


#
# data



default_var_group <- function(dic = NULL) {

  criterios <- c("Cat", "Dat", "Yea")
  var_group <- NULL
  for (criterio in criterios) {
    var_group_temp <- dsdatawiz:::guess_vars(dic, criterio)[[paste0("var_", tolower(criterio))]]
    if (!is.null(var_group_temp) && length(var_group_temp) > 0) {
      var_group <- var_group_temp
      break
    }
  }

  var_group
}

#' @keywords internal
complete_values <- function(data, var_find = NULL, var_expand = NULL, var_num = NULL) {
  if (ncol(data) < 2) {
    stop("data must have at least two columns.")
  }
  var_num <- var_num %||% names(data)[3]
  var_find <- var_find %||% names(data)[1]
  var_expand <- var_expand %||% names(data)[2]

  all_vars <- c(var_find, var_expand, var_num)
  if (!all(all_vars %in% names(data))) {
    stop("One or more specified variables do not exist in the data frame.")
  }

  data <- data |>
    dplyr::as_tibble() |>
    tidyr::complete(!!!rlang::syms(var_find),
                    !!rlang::sym(var_expand),
                    fill = setNames(list(NA), var_num))

  data
}






hdtype_viz <- function(var_cat = NULL, var_num = NULL,
                       var_dat = NULL, var_yea = NULL,
                       var_img = NULL) {

  parts <- list()

  if (!is.null(var_cat)) parts <- c(parts, rep("Cat", length(var_cat)))
  if (!is.null(var_yea)) parts <- c(parts, rep("Yea", length(var_yea)))
  if (!is.null(var_dat)) parts <- c(parts, rep("Dat", length(var_dat)))
  if (!is.null(var_img)) parts <- c(parts, rep("Img", length(var_img)))
  if (!is.null(var_num)) parts <- c(parts, rep("Num", length(var_num)))

  hdtype <- paste(parts, collapse = "")
  hdtype
}

data_vars <- function(data) {
  data_names <- names(hdtable(data)$data)
  data_names
}
