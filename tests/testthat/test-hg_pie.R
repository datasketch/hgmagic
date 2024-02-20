test_that("hg_pie", {
  hg_pie(data = iris, var_num = "sepal_length", var_cat = "species")
  hg_pie(data = iris, var_cat = "species")
})

test_that("hg_pie_CatNum", {
  data <- iris |> select(Species, Sepal.Length)
  hg_pie_CatNum(data)
})

test_that("hg_pie_Cat", {
  data <- iris |> select(Species, Sepal.Length)
  hg_pie_Cat(data, legend_show = TRUE)
})
