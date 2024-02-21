test_that("hg_donut", {
  hg_donut(data = iris, var_num = "sepal_length", var_cat = "species")
  hg_donut(data = iris, var_cat = "species")
})

test_that("hg_donut_CatNum", {
  data <- iris |> select(Species, Sepal.Length)
  hg_donut_CatNum(data, legend_show = TRUE)
})

test_that("hg_donut_Cat", {
  data <- iris |> select(Species, Sepal.Length)
  hg_donut_Cat(data, legend_show = TRUE)
})
