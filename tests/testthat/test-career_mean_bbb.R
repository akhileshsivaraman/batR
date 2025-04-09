test_that("career_mean_bbb returns a dataframe with 3 columns: ball, mean scored, mean SR", {
  d <- read_rds(test_path("fixtures", "mens_innings_list.RDS"))
  ball_by_ball_data <- career_bbb(d)
  ball_by_ball_mean <- career_mean_bbb(ball_by_ball_data)
  
  expect_s3_class(ball_by_ball_mean, "data.frame")
  expect_contains(colnames(ball_by_ball_mean), c("ball", "mean scored", "mean SR"))
})

test_that("the number of observations in the dataframe produced by career_mean_bbb is equal to the longest innings played by the batter", {
  d <- read_rds(test_path("fixtures", "mens_innings_list.RDS"))
  ball_by_ball_data <- career_bbb(d)
  ball_by_ball_mean <- career_mean_bbb(ball_by_ball_data)
  
  longest_innings <- colnames(ball_by_ball_data) |> # i.e. most number of balls played in an innings
    last() |>
    as.integer()
  
  expect_equal(longest_innings, nrow(ball_by_ball_mean))
})

# test the mean calculations are working? will need to create fake data
test_that("career_mean_bbb calculates mean scored and mean SR correctly", {
  ball_by_ball_data <- tibble(
    match_id = c("match_1", "match_2", "match_3"),
    tournament = "tournament",
    `1` = c(4, 2, 0),
    `2` = c(0, 1, 3),
    `3` = c(0, 6, 1)
  )
  
  ball_by_ball_mean <- career_mean_bbb(ball_by_ball_data)
  
  expected_result <- tibble(
    ball = c(1, 2, 3),
    `mean scored` = c(2.00, 1.33, 2.33),
    `mean SR` = c(200, 133, 233)
  )
  
  expect_equal(ball_by_ball_mean, expected_result)
})