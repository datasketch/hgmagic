test_that("bar_negative_stack", {
})

test_that("bar_negative_stack_CatCat", {
  data <- sample_data(
    "Cat-Cat",
    names = c("cat1", "cat2"),
    nlevels = 10,
    n = 100,
    addNA = FALSE,
    rep = TRUE
  )

  hg_bar_negative_stack_CatCat(data)
})

test_that("bar_negative_stack_CatCatNum", {
  data <- sample_data(
    "Cat-Cat-Num",
    names = c("cat1", "cat2", "value"),
    gt0 = TRUE,
    nlevels = 10,
    n = 100,
    addNA = FALSE,
    rep = TRUE
  )

  hg_bar_negative_stack_CatCatNum(
    data, agg = "sum",
    legend_align = "right",
    legend_orientation = "vertical",
    legend_vertical_align = "middle"
  )
  hg_bar_negative_stack_CatCatNum(data, agg = "mean")
})
