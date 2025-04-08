test_that("find_bbb returns a list of S3 dataframes", {
  test_con <- DBI::dbConnect(duckdb(), test_path("fixtures", "test_data.duckdb"))
  
  x <- find_bbb(
    "CH Gayle",
    "male",
    test_con
  )
  
  expect_type(x, "list")
  map(x, \(.x) expect_s3_class(.x, "data.frame"))
})
