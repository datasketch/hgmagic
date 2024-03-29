test_that("hg_line", {
  data <- sample_data('Dat-Num', names = c('date', 'value'))
  hg_line(data = data, var_dat = 'date', var_num = 'value')

  data <- sample_data('Cat-Dat-Num', names = c('cats', 'date', 'value'))
  hg_line(data = data, var_cat = 'cats', var_dat = 'date', var_num = 'value')
})

test_that("hg_line_Dat", {
  data <- sample_data('Dat', names = 'date', rep = TRUE)
  hg_line_Dat(data = data)
})

test_that("hg_line_CatDat", {
  data <- sample_data('Cat-Dat', names = c('cat','date'), rep = TRUE, n = 50)
  hg_line_CatDat(data = data)
})

test_that("hg_line_CatDatNum", {
  data <- sample_data('Cat-Dat-Num', names = c('cat','date', 'value'), rep = TRUE, n = 50)
  hg_line_CatDatNum(data = data)
})

test_that("hg_line_DatNumNum", {
  data <- sample_data('Dat-Num-Num', names = c('date', 'value1', 'value2'), rep = TRUE, n = 50)
  hg_line_DatNumNum(data = data)
})
