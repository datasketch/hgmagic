test_that("hg_dependency_wheel_CatCat", {
  data <- sample_data("Cat-Cat", n = 100, names = c("cat1", "cat2"))
  hg_dependency_wheel_CatCat(data = data)
})

test_that("hg_dependency_wheel_CatCatNum", {
  data <- sample_data("Cat-Cat-Num", n = 100, names = c("cat1", "cat2", "num"), nlevels = 15)
  hg_dependency_wheel_CatCatNum(data = data)

  data_agg <- data |>
    dplyr::group_by(cat1, cat2) |>
    dplyr::summarise(num = sum(num, na.rm = TRUE))

  c3 <- sample_data("Cat", n = nrow(data_agg), names = c("cat3"), addNA = FALSE)
  data2 <- data_agg |> bind_cols(c3)

  hg_dependency_wheel_CatCatNum(
    data = data2,
    tooltip_template = "{cat1} ⭢ {cat2}<br><b>Valor:</b> {num}<br><b>Texto:</b> {cat3}",
    tooltip_add_unique_cats = "cat3"
  )
})
