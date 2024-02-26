test_that("hc_treemap", {
  hg_treemap(data = iris, var_num = "sepal_length", var_cat = "species")
  hg_treemap(data = starwars, var_cat = c("sex", "hair_color"), datalabel_show = T)
})
