test_that("career_mean_bbb returns a dataframe with 3 columns: ball, mean scored, mean SR", {
  d <- read_rds(test_path("fixtures", "mens_innings_list.RDS"))
  ball_by_ball_data <- career_bbb(d)
  y <- career_mean_bbb(ball_by_ball_data)
  
  expect_s3_class(y, "data.frame")
  expect_contains(colnames(y), c("ball", "mean scored", "mean SR"))
})

test_that("the number of observations in the dataframe produced by career_mean_bbb is equal to the longest innings played by the batter", {
  d <- read_rds(test_path("fixtures", "mens_innings_list.RDS"))
  ball_by_ball_data <- career_bbb(d)
  y <- career_mean_bbb(ball_by_ball_data)
  
  longest_innings <- colnames(ball_by_ball_data) |> # i.e. most number of balls played in an innings
    last() |>
    as.integer()
  
  expect_equal(longest_innings, nrow(y))
})