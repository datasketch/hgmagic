test_that("hg_line", {
  data <- sample_data('Dat-Num', names = c('date', 'value'))
  hg_line(data = data, var_dat = 'date', var_num = 'value', drop_na = TRUE)

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
  hg_line_CatDatNum(data = data,
                    format_sample_num = "1 345,0",
                    legend_align = "right",
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
  hg_line_NumNum(data = data, format_sample_num = "1 345,0")
})

test_that("hg_line_CatNumNum", {
  data <- sample_data('Cat-Num-Num', names = c('cat', 'value1', 'value2'), rep = TRUE)
  hg_line_CatNumNum(data = data)
})

test_that("hg_line completes dates", {
  df <- tibble(
    date = c("2024-12-15", "2024-12-20", "2024-12-25", "2025-01-10"),
    value = c(10, 15, 20, 25)
  )

  hg_line_DatNum(df, format_sample_dat = "%m-%d-%Y")
  hg_line_DatNum(df, line_fix_missing = TRUE, format_sample_dat = "%m-%d-%Y")
  hg_line_DatNum(df, line_fix_missing = TRUE, line_connect_na = TRUE)

  df <- tibble(
    cat = c('red', 'red','blue', 'blue'),
    date = as.Date(c("2024-12-15", "2024-12-20", "2024-12-25", "2025-01-10")),
    value = c(10, 15, 20, 25)
  )

  hg_line_CatDatNum(df, format_sample_dat = "%m-%d-%Y")
  hg_line_CatDatNum(df, line_fix_missing = TRUE, format_sample_dat = "%m-%d-%Y")
  hg_line_CatDatNum(df, line_fix_missing = TRUE, line_connect_na = TRUE)
})

test_that("hg_line formats dates", {
  df <- tibble(
    ddsate = as.Date(c("2024-12-15", "2024-12-20", "2024-12-25", "2024-12-25", "2025-01-10", "2025-02-10", "2025-02-10")),
    value = c(10, 15, NA, 20, 25, NA, NA)
  )

  hg_line_DatNum(df, format_sample_dat = "%m-%d-%Y")
  hg_line_DatNum(df, line_fix_missing = TRUE, format_sample_dat = "%m-%d-%Y")
  hg_line_DatNum(df, format_sample_dat = "%b %d, %y", axis_text_wrap = 20)
  hg_line_DatNum(
    df, line_connect_na = TRUE, line_fix_missing = TRUE,
    format_sample_dat = "%b %d, %y", axis_text_wrap = 20
  )
  hg_line_DatNum(
    df, line_connect_na = TRUE, line_fix_missing = TRUE,
    format_sample_dat = "%B %d, %y", axis_text_wrap = 20
  )
})
