test_that("hg_sunburst", {
  hg_sunburst(data = mpg, var_cat = c("manufacturer", "model"))
})

test_that("hg_sunburst_CatCat", {
  data <- mpg
  hg_sunburst_CatCat(data, title = "holis")
})
