test_that("hg_scatter", {
  data(mpg, package = "ggplot2")
})

test_that("hg_scatter_NumNum", {
  data <- sample_data("Num-Num", names = c("value1", "value2"))

  hg_scatter_NumNum(data)
})

test_that("hg_scatter_CatNumNum", {
  data <- sample_data("Cat-Num-Num", names = c("cat", "value1", "value2"))

  hg_scatter_CatNumNum(data, agg = NULL)
  hg_scatter_CatNumNum(data, agg = "sum")
  hg_scatter_CatNumNum(data, agg = "mean")
})

test_that("hg_scatter_CatNumNumNum", {
  data <- sample_data(
    "Cat-Num-Num",
    names = c("cat", "value1", "value2"),
    nlevels = 10,
    addNA = FALSE
  )
  data$value3 <- runif(20, 5, 15)

  hg_scatter_CatNumNumNum(data, agg = NULL)
  hg_scatter_CatNumNumNum(data, agg = "sum")
  hg_scatter_CatNumNumNum(data, agg = "mean")
})

test_that("hg_dots_CatCatNum", {
  data <- sample_data("Cat-Cat-Num", names = c("cat1", "cat2", "value"))

  hg_dots_CatCatNum(data, agg = NULL)
  hg_dots_CatCatNum(data, agg = "sum")
  hg_dots_CatCatNum(data, agg = "mean")
})
