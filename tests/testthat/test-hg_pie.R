test_that("hg_pie", {
  hg_pie(data = iris, var_num = "sepal_length", var_cat = "species")
  hg_pie(data = iris, var_cat = "species")
})

test_that("hg_pie_CatNum", {
  data <- iris |> select(Species, Sepal.Length)
  hg_pie_CatNum(data)

  # Examples with datalabels
  hg_pie_CatNum(
    data,
    datalabel_show = TRUE,
    datalabel_size = 20,
    datalabel_color = "white",
    datalabel_text_outline_show = FALSE
  )

  hg_pie_CatNum(
    data,
    datalabel_show = TRUE,
    datalabel_size = 40,
    datalabel_color = "contrast",
    datalabel_text_outline_show = TRUE
  )
})

test_that("hg_pie_Cat", {
  data <- iris |> select(Species, Sepal.Length)
  hg_pie_Cat(data, legend_show = TRUE)
})
