#---- create innings_list RDS ----
con <- dbConnect(duckdb(), "tests/testthat/fixtures/test_data.duckdb")

mens_innings_list <- find_bbb(
  "CH Gayle",
  "male",
  con
)

womens_innings_list <- find_bbb(
  "S Mandhana",
  "female",
  con
)

saveRDS(mens_innings_list, "tests/testthat/fixtures/mens_innings_list.RDS")
saveRDS(womens_innings_list, "tests/testthat/fixtures/womens_innings_list.RDS")
