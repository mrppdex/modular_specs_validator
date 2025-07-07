library(testthat)
library(stringr)

source(file.path('..','..','modules','utils.R'))

test_that("lowercase_query_vars lowercases variable names", {
  expect_equal(
    lowercase_query_vars("VAR1=='X' & is.na(VAR2)"),
    "var1=='X' & is.na(var2)"
  )
})
