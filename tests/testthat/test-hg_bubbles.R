test_that("hg_bubbles", {
  df <- iris
  hg_bubbles(df, var_cat = "Species")
})

test_that("hg_bubbles_Cat", {
  data <- iris |> select(Species)
  hg_bubbles_Cat(data)
  hg_bubbles_Cat(data, color_by = "species")
})

test_that("hg_bubbles_CatNum", {
  data <- iris |> select(Species, Sepal.Length)
  hg_bubbles_CatNum(data)
})

test_that("hg_bubbles_CatCat", {
  data <- starwars |> select(sex, hair_color)
  hg_bubbles_CatCat(data)
  hg_bubbles_CatCat(data, bubble_cluster = FALSE)
})

test_that("hg_bubbles_CatCatNum", {
  data <- starwars |> select(sex, hair_color, height)

  hg_bubbles_CatCatNum(data)
  hg_bubbles_CatCatNum(data, bubble_min = "10%", bubble_max = "50%")
})
