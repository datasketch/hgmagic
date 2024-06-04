test_that("hg_sankey_CatCatNum", {
  st <- starwars |>
    select(name, eye_color, birth_year) |>
    head(20)
  hg_sankey_CatCatNum(data = st)
})
