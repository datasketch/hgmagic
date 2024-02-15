test_that("data_prep", {

  # CatNum
  ht_iris <- hdtable(iris)
  data_result <- data_prep(ht_iris$data,
                           ht_iris$dic,
                           "species",
                           "sepal_width")

  # CatCatNum
  data_result <- data_prep(starwars, NULL,
                    var_group = c("hair_color"),
                    c("mass", "height"))



})

test_that("default_var_group", {

  data_result <- default_var_group(hdtable(iris)$dic)

})

test_that("completevalues", {

  ht_data <- sample_data("Cat-Dat-Num")
  data_result <- completevalues(ht_data)

})

test_that("hdtype_viz", {
  hdt
})
