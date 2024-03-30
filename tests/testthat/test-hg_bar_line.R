test_that("multiplication works", {

  meses <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio',
             'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
  ventas <- c(120, 150, 180, 200, 210, 200, 230, 220, 210, 230, 240, 250)
  crecimiento <- c(NA, 25, 20, 11, 5, -5, 15, -4, -5, 9, 4, 4)
  df <- tibble(meses, ventas, crecimiento)
  hg_bar_line(df, var_cat = "meses", var_num = c("ventas", "crecimiento"))

})
