test_that("hg_radial_bar", {
  hg_radial_bar(data = iris, var_num = "sepal_length", var_cat = "species")
  hg_radial_bar(data = iris, var_cat = "species")

  # add titles and colors
  hg_radial_bar(
    data = iris,
    var_cat = "species",
    var_num = "sepal_length",
    color_by = "species",
    title = "This is a title",
    subtitle = "This is a subtitle"
  )

  hg_radial_bar(data = starwars, var_cat = c("sex", "hair_color"))
  hg_radial_bar(data = starwars, var_cat = c("sex", "hair_color"), bar_graph_type = "stacked")
})

test_that("hg_radial_bar_Cat", {
  data <- iris |> select(Species, Sepal.Length)
  hg_radial_bar_Cat(data)
})

test_that("hg_radial_bar_CatNum", {
  data <- iris |> select(Species, Sepal.Length)
  hg_radial_bar_CatNum(data)
})

test_that("hg_radial_bar_CatCat", {
  data <- starwars |> select(sex, hair_color)
  hg_radial_bar_CatCat(data)
})

test_that("hg_radial_bar_CatCatNum", {
  data <- starwars |> select(hair_color, sex, height)

  hg_radial_bar_CatCatNum(
    data,
    drop_na = TRUE,
    color_by = "hair_color",
    bar_graph_type = "stacked",
    legend_align = "right",
    legend_orientation = "vertical",
    legend_vertical_align = "middle"
  )

  data <- sample_data("Cat-Cat-Num", n = 100, names = c("cat1", "cat2", "num"))

  hg_radial_bar_CatCatNum(
    data,
    color_by = "cat1",
    drop_na = TRUE,
    sort = "desc",
    sort_intra_cat = FALSE,
    axis_line_y_size = 0,
    grid_x_width = 0,
    grid_y_width = 0,
    # datalabel_show = TRUE,
    bar_graph_type = "stacked",
    legend_align = "right",
    legend_orientation = "vertical",
    legend_vertical_align = "middle"
  )
})
