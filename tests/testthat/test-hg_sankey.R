test_that("hg_sankey_CatCatNum", {
  st <- starwars |>
    select(name, eye_color, birth_year)

  hg_sankey_CatCatNum(data = st)
})

test_that("hg_sankey_CatCat", {
  st <- starwars |>
    select(name, eye_color, birth_year)

  hg_sankey_CatCat(data = st)
})

test_that("hg_sankey_CatCatCatNum", {
  st <- starwars |>
    select(sex, eye_color, gender, birth_year)

  hg_sankey_CatCatCatNum(data = st)

  # With agg = min
  hg_sankey_CatCatCatNum(data = st, agg = "min")
})

test_that("hg_sankey_CatCatCat", {
  st <- starwars |>
    select(sex, eye_color, gender, birth_year)

  hg_sankey_CatCatCat(data = st)
})
