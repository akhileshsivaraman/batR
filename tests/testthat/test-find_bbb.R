test_that("find_bbb returns a list of S3 dataframes", {
  test_con <- DBI::dbConnect(duckdb(), test_path("fixtures", "test_data.duckdb"))
  on.exit(DBI::dbDisconnect(test_con))
  
  male_data <- find_bbb(
    "CH Gayle",
    "male",
    test_con
  )
  
  female_data <- find_bbb(
    "S Mandhana",
    "female",
    test_con
  )
  
  expect_type(male_data, "list")
  map(male_data, \(.x) expect_s3_class(.x, "data.frame"))
  
  expect_type(female_data, "list")
  map(female_data, \(.x) expect_s3_class(.x, "data.frame"))
})

test_that("find_bbb returns an empty list when the gender is incorrect", {
  test_con <- DBI::dbConnect(duckdb(), test_path("fixtures", "test_data.duckdb"))
  on.exit(DBI::dbDisconnect(test_con))
  
  female_data <- find_bbb(
    "S Mandhana",
    "male",
    test_con
  )
  
  expect_length(female_data, 0)
})
