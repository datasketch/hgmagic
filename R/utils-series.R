#' @export
hc_data_series <- function(hc, data) {

  is_list_of_series <- is.list(data)
  list_in_list <- all(sapply(data, function(x) is.list(x) && !is.null(x$data)))

  # Si los datos son una lista de series, utiliza hc_add_series_list o hc_series dependiendo de la estructura
  if (is_list_of_series) {
    if (list_in_list) {
      return(hc_add_series_list(hc, data))
    } else {
      # Si es una única serie pero aún así es una lista, utiliza hc_series
      return(hc_series(hc, list(data = data)))
    }
  } else {
    # Para un único conjunto de datos que no está dentro de una lista de series
    return(hc_add_series(hc, data))
  }
}
