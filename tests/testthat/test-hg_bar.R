test_that("hg_bar", {
  hg_bar(data = iris, var_num = "sepal_length")
  hg_bar_Num(data = iris)
  hg_bar(data = iris, var_num = "sepal_length",
         var_cat = "species", sort = "asc", format_sample_cat = "UPPER")
  hg_bar(data = iris, var_num = "sepal_length",
         var_cat = "species", sort = "desc")
  hg_bar(data = iris, var_cat = "species", color_by = "species")

  # add titles
  hg_bar(data = iris, var_num = "sepal_length", var_cat = "species",
         title = "This is a title",
         subtitle = "This is a subtitle",
         title_axis_x = "x axis",
         title_axis_y = "y axis")


  hg_bar(data = starwars, var_cat = c("sex", "hair_color"), agg_text = "Conteo de color según género")
  hg_bar(data = starwars, var_cat = c("sex", "hair_color"), var_num = "height",
         sort = "asc", slice_n = 3)
  hg_bar(data = starwars, var_cat = c("sex", "hair_color"), var_num = "height",
         sort = "desc", axis_y_format_sample_num = "1.234,", format_sample_cat = "Titulo")
  hg_bar(data = starwars, var_cat = c("sex", "hair_color"),
         bar_graph_type = "stacked", sort = "desc", order_var1 = c("female", "male"))
  hg_bar(data = starwars, var_cat = c("sex"),
         var_num = c("height", "mass"), format_sample_num = "1,234.")

})

test_that("hg_bar_CatNum", {
  data <- iris |> select(Species, Sepal.Length)
  hg_bar_CatNum(data)
})

test_that("hg_bar_Cat", {
  data <- iris |> select(Species, Sepal.Length)
  hg_bar_Cat(data)
})


test_that("hg_bar_CatCatNum", {
  data <- starwars |> select(hair_color, sex, height)
  hg_bar_CatCatNum(data, legend_align = "right",
                   legend_orientation = "vertical",
                   agg = "mean",
                   legend_vertical_align = "middle",
                   sort = "desc", sort_intra_cat = T)
})

test_that("hg_bar_CatCat", {
  data <- starwars |> select(sex, hair_color)
  hg_bar_CatCat(data)
})

test_that("hg_bar_CatNumNum", {
  data <- starwars |> select(sex, height, mass)
  hg_bar_CatNumNum(data)
2})

