test_that("hc_treemap", {
  hg_treemap(data = iris, var_num = "sepal_length", var_cat = "species")
  hg_treemap(data = starwars, var_cat = c("sex", "hair_color"), datalabel_show = T)

  data_sample <- sample_data("Cat-Num", names = c("cat", "value"))
  hg_treemap_CatNum(data_sample)
  data_sample <- sample_data("Cat-Cat-Num", names = c("cat", "cat2", "value"))
  hg_treemap_CatCatNum(data_sample)
})
