test_that("hg_dumbbell_CatNumNum", {
  data <- data.frame(
    Country = c('Austria', 'Belgium', 'Czechia', 'Estonia', 'Greece', 'Hungary', 'Iceland', 'Lithuania', 'Norway', 'Portugal', 'Romania', 'Slovakia', 'Sweden', 'Switzerland'),
    Temperature = c(70.1, 71.0, 69.6, 70.4, 73.8, 69.2, 73.8, 71.1, 74.3, 66.7, 68.2, 69.8, 74.7, 73.2),
    Altitude = c(81.3, 81.9, 77.4, 76.9, 80.3, 74.5, 83.2, 74.5, 83.2, 81.2, 72.9, 74.8, 83.2, 84.0)
  )


  hg_dumbbell_CatNumNum(data, title_axis_y = "Life Experience Change", title_axis_x = "Paises")
})
