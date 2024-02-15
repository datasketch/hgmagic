#' @export
hc_add_options <- function(hc, viz, user_options = list(), common_options = list()) {
  final_options <- modifyList(default_options[[viz]], user_options)
  args_list <- list(hc)
  args_list[[viz]] <- final_options

  hc <- do.call("hc_plotOptions", args_list)
  hc
}



default_options <- list(
  bar = list(
    stacking = "normal",
    dataLabels = list(enabled = TRUE)
  ),
  line = list(
    color = "blue",
    marker = list(
      fillColor = "white",
      lineWidth = 2,
      lineColor = NULL
    )
  )
)



common_options <- list(
  enableMouseTracking = TRUE
)

# bar_options <- function(opts) {
#
# }
