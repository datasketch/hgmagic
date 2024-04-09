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
  hg_line_CatDatNum(data = data, legend_align = "right",
                    legend_orientation = "vertical",
                    legend_vertical_align = "middle")
})

test_that("hg_line_DatNumNum", {
  data <- sample_data('Dat-Num-Num', names = c('date', 'value1', 'value2'), rep = TRUE, n = 50)
  hg_line_DatNumNum(data = data)
})

test_that("hg_line_Num", {
  data <- sample_data('Num', names = 'value', rep = TRUE)
  hg_line_Num(data = data)
})

test_that("hg_line_NumNum", {
  data <- sample_data('Num-Num', names = c('value1', 'value2'), rep = TRUE)
  hg_line_NumNum(data = data)
})

test_that("hg_line_CatNumNum", {
  data <- sample_data('Cat-Num-Num', names = c('cat', 'value1', 'value2'), rep = TRUE)
  hg_line_CatNumNum(data = data)
})
