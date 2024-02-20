test_that("hg_bar", {
  hg_bar(data = iris, var_num = "sepal_length", var_cat = "species")
  hg_bar(data = iris, var_cat = "species")

  # add titles
  hg_bar(data = iris, var_num = "sepal_length", var_cat = "species",
         title = "This is a title",
         subtitle = "This is a subtitle",
         title_axis_x = "x axis",
         title_axis_y = "y axis")

  hg_bar(data = starwars, var_cat = c("sex", "hair_color"))
  hg_bar(data = starwars, var_cat = c("sex", "hair_color"), bar_graph_type = "stacked")
})
