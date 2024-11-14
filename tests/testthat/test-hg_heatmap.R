test_that("hg_heatmap", {
  hg_heatmap(starwars, var_cat = c("hair_color", "gender"))

  hg_heatmap(
    starwars, var_cat = c("hair_color", "gender"), var_num = "height",
    agg = "mean", title_axis_x = "Hair Color", title_axis_y = "Gender",
    title = "Starwars Characters Mean Height by Hair Color and Gender"
  )
})

test_that("hg_heatmap_CatCat", {
  data <- sample_data("Cat-Cat", n = 100, names = c("cat1", "cat2"))
  hg_heatmap_CatCat(data)
})

test_that("hg_heatmap_CatCatNum", {
  data <- sample_data("Cat-Cat-Num", n = 100, names = c("cat1", "cat2", "num"))
  hg_heatmap_CatCatNum(data)
})
