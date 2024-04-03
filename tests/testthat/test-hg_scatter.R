test_that("hg_scatter", {
  data(mpg, package = "ggplot2")
})

test_that("hg_scatter_NumNum", {
  data <- sample_data("Num-Num", names = c("value1", "value2"))

  hg_scatter_NumNum(data)
})

test_that("hg_scatter_CatNumNum", {
})

test_that("hg_scatter_CatCatNum", {
})
