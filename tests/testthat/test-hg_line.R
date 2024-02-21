test_that("hg_line", {
  data <- sample_data('Dat-Num', names = c('date', 'value'))
  hg_line(data = data, var_dat = 'date', var_num = 'value')

  data <- sample_data('Cat-Dat-Num', names = c('cats', 'date', 'value'))
  hg_line(data = data, var_cat = 'cats', var_dat = 'date', var_num = 'value')
})
