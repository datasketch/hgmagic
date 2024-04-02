test_that("hg_bar_grid", {
  data(mpg, package = "ggplot2")

  hg_bar_grid(mpg, var_cat = c("class", "manufacturer"))
  hg_bar_grid(mpg, var_cat = c("class", "manufacturer"), var_num = "displ")
})

test_that("hg_bar_grid_CatCat", {
  data <- sample_data("Cat-Cat", names = c("cat1", "cat2"), rep = TRUE)

  hg_bar_grid_CatCat(data)
})

test_that("hg_bar_grid_CatCatNum", {
  data <- sample_data(
    "Cat-Cat-Num",
    names = c("cat1", "cat2", "value"),
    rep = TRUE
  )

  hg_bar_grid_CatCatNum(data, agg = "sum")
  hg_bar_grid_CatCatNum(data, agg = "mean")
})

test_that("hg_bar_grid_CatCatCat", {
  data <- sample_data(
    "Cat-Cat-Cat",
    names = c("cat1", "cat2", "cat3"),
    rep = TRUE,
    n = 100,
    nlevels = 3,
    addNA = FALSE
  )

  hg_bar_grid_CatCatCat(data)
})

test_that("hg_bar_grid_CatCatCatNum", {
  data <- sample_data(
    "Cat-Cat-Cat-Num",
    names = c("cat1", "cat2", "cat3", "value"),
    rep = TRUE,
    n = 100,
    nlevels = 3,
    addNA = FALSE
  )

  hg_bar_grid_CatCatCatNum(data, agg = "sum")
  hg_bar_grid_CatCatCatNum(data, agg = "mean")
})
