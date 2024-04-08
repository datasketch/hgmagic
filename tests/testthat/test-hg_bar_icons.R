names <- c("Petronas", "Pisa", "Eiffel Tower", "Ahu-tongariki")
images <- c(
  "https://www.svgrepo.com/show/27082/petronas-towers.svg",
  "https://www.svgrepo.com/show/1171/tower-of-pisa.svg",
  "https://www.svgrepo.com/show/19456/tokyo-tower.svg",
  "https://www.svgrepo.com/show/27081/ahu-tongariki.svg"
)
path <- c(
  "inst/images/petronas-towers.svg",
  "inst/images/tower-of-pisa.svg",
  "inst/images/tokyo-tower.svg",
  "inst/images/ahu-tongariki.svg"
)
values <- c(100, 150, 200, 250)

data_remote <- tibble(name = names, image = images, value = values)
data_local <- tibble(name = names, image = path, value = values)

test_that("hg_bar_icons", {
})

test_that("hg_bar_icons_CatImgNum", {
  hg_bar_icons_CatImgNum(data_remote)
  hg_bar_icons_CatImgNum(data_local)
})
