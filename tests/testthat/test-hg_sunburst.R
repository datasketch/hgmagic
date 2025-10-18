test_that("hg_sunburst", {
  library(ggplot2)
  hg_sunburst(data = mpg, var_cat = c("manufacturer", "model"))
})

test_that("hg_sunburst_CatCat", {
  data <- mpg
  hg_sunburst_CatCat(data, format_sample_cat = "HOLIS")
})
