process_CatNum <- function(d, viz) {
  if (viz %in% c("bar", "pie", "donut")) {
    data <- purrr::pmap(.l = list(d[[1]], d[[2]], d[[3]], d[[4]]),
                        .f = function(name, y, label, color) {
                          list("name" = as.character(name),
                               "y" = as.numeric(y),
                               "label" = label,
                               "color" = color
                          )
                        })

    data <- list(data = data)
  }
  data
}
