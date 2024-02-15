test_that("hg_bar", {
  hg_bar(data = iris, var_num = "sepal_length", var_cat = "species")
  hg_bar(data = iris, var_cat = "species")
  hg_bar(data = starwars, var_cat = c("sex", "hair_color"))
})
