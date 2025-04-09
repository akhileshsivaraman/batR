test_that("condense_career returns a dataframe with 1 row", {
  d <- read_rds(test_path("fixtures", "womens_innings_list.RDS"))
  x <- condense_career(d[[1]])
  
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1)
})

test_that("the length of the dataframe returned by condense career - 2 = n_rows of the df supplied - wides", {
  d <- read_rds(test_path("fixtures", "mens_innings_list.RDS"))
  
  df_supplied <- d[[1]]
  n_wides <- df_supplied |>
    filter(!is.na(wides)) |>
    tally() |>
    pull(n)
  
  x <- condense_career(df_supplied)
  
  expect_equal(length(x) - 2, nrow(df_supplied) - n_wides)
})


test_that("career_bbb returns a dataframe with n_rows equal to the length of the list supplied", {
  d <- read_rds(test_path("fixtures", "mens_innings_list.RDS"))
  
  x <- career_bbb(d)
  
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), length(d))
})
