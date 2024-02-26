test_that("hc_treemap", {
  hg_treemap(data = iris, var_num = "sepal_length", var_cat = "species")
})
