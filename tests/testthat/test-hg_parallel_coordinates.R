test_that("hg_parallel_coordinates_CatCatCatCatCatCatCat", {

  data <- starwars |>
    select(hair_color, skin_color, eye_color, sex, gender, homeworld, species)

  hg_parallel_coordinates_CatCatCatCatCatCatCat(data = data)
})
