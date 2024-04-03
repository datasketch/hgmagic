test_that("hg_scatter", {
  data(mpg, package = "ggplot2")
})

test_that("hg_scatter_NumNum", {
  data <- sample_data("Num-Num", names = c("value1", "value2"))

  hg_scatter_NumNum(data)
})

test_that("hg_scatter_CatNumNum", {
  data <- sample_data("Cat-Num-Num", names = c("cat", "value1", "value2"))

  hg_scatter_CatNumNum(data)
  hg_scatter_CatNumNum(data, scatter_agg = TRUE) # fix this
})

test_that("hg_scatter_CatCatNum", {
  data <- sample_data("Cat-Cat-Num", names = c("cat1", "cat2", "value"))

  hg_scatter_CatCatNum(data)
})
