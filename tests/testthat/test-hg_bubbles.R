test_that("hg_bubbles", {
  df <- iris
  hg_bubbles(df, var_cat = "Species")
})
