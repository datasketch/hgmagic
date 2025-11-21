test_that("data_processing", {

  # CatNum
  ht_iris <- hdtable(iris)
  data_result <- data_processing(ht_iris$data,
                           ht_iris$dic,
                           "species",
                           "sepal_width")

  # CatCatNum
  data_result <- data_processing(starwars, NULL,
                    var_group = c("hair_color"),
                    c("mass", "height"))



})

test_that("default_var_group", {

  data_result <- default_var_group(hdtable(iris)$dic)

})

test_that("complete_values", {

  ht_data <- sample_data("Cat-Dat-Num")
  data_result <- complete_values(ht_data)

})

test_that("hdtype_viz", {
  result <- hdtype_viz(var_cat = "species", var_num = "value")
  expect_equal(result, "CatNum")
})
