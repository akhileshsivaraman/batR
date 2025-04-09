#---- create test duckdb ----
con <- DBI::dbConnect(duckdb::duckdb(), "data/t20_batting_data.duckdb")

# filter the table to get the data for a player
male_player <- DBI::dbGetQuery(
  con,
  "SELECT * FROM mens_ball_by_ball_data WHERE striker = 'CH Gayle'"
)

female_player <- DBI::dbGetQuery(
  con,
  "SELECT * FROM womens_ball_by_ball_data WHERE striker = 'S Mandhana'"
)

DBI::dbDisconnect(con)

# load into test db
test_con <- DBI::dbConnect(duckdb::duckdb(), "tests/testthat/fixtures/test_data.duckdb")
DBI::dbWriteTable(test_con, "mens_ball_by_ball_data", male_player)
DBI::dbWriteTable(test_con, "womens_ball_by_ball_data", female_player)
DBI::dbDisconnect(test_con)
