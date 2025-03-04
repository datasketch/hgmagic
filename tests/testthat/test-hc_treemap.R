test_that("hc_treemap", {
  hg_treemap(data = iris, var_num = "sepal_length", var_cat = "species")
  hg_treemap(data = iris, var_num = "sepal_length", var_cat = "species", color_palette_type = "sequential")
  hg_treemap(data = starwars, var_cat = c("sex", "hair_color"), datalabel_show = T)
  hg_treemap(data = starwars, var_cat = c("sex", "hair_color"), datalabel_show = T, color_palette_type = "sequential")

  data_sample <- sample_data("Cat-Num", names = c("cat", "value"))
  hg_treemap_CatNum(data_sample)
  hg_treemap_CatNum(data_sample, color_palette_type = "sequential")
  data_sample <- sample_data("Cat-Cat-Num", names = c("cat", "cat2", "value"))
  hg_treemap_CatCatNum(data_sample)
  hg_treemap_CatCatNum(data_sample, color_palette_type = "sequential")
})
