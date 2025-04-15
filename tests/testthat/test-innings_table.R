test_that("innings_table correctly calculates balls faced, number of runs scored and SR for each innings", {
  ball_by_ball_data <- tibble(
    match_id = c("match_1", "match_2", "match_3"),
    tournament = "tournament",
    `1` = c(4, 2, 1),
    `2` = c(0, 1, 3),
    `3` = c(0, 6, 0),
    `4` = c(2, 0, NA),
    `5` = c(6, NA, NA)
  )
  
  player_innings <- innings_table(ball_by_ball_data)
  
  expected_result <- tibble(
    match_id = as.factor(c("match_1", "match_2", "match_3")),
    `balls faced` = c(5, 4, 3),
    `total scored` = c(12, 9, 4),
    SR = round(c(12/5, 9/4, 4/3) * 100, 2)
  )
  
  expect_equal(player_innings, expected_result)
})
