data <- tibble(
  name = c("Petronas", "Pisa", "Eiffel Tower", "Ahu-tongariki"),
  image = c(
    "https://www.svgrepo.com/show/27082/petronas-towers.svg",
    "https://www.svgrepo.com/show/1171/tower-of-pisa.svg",
    "https://www.svgrepo.com/show/19456/tokyo-tower.svg",
    "https://www.svgrepo.com/show/27081/ahu-tongariki.svg"
  ),
  value = c(100, 150, 200, 250)
)

test_that("hg_bar_icons", {
})

test_that("hg_bar_icons_CatImgNum", {
  hg_bar_icons_CatImgNum(data)
})
