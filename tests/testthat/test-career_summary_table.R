test_that("career_summary_table calculates summary statistics as expected", {
  # set up test data
  ball_by_ball_data <- tibble(
    match_id = c("match_1", "match_2", "match_3"),
    tournament = "tournament",
    `1` = c(4, 2, 1),
    `2` = c(0, 1, 3),
    `3` = c(0, 6, 4),
    `4` = c(2, 0, NA),
    `5` = c(6, NA, NA)
  )
  
  player_innings <- tibble(
    match_id = as.factor(c("match_1", "match_2", "match_3")),
    `balls faced` = c(5, 4, 3),
    `total scored` = c(12, 9, 8),
    SR = round(c(12/5, 9/4, 8/3) * 100, 2)
  )
  
  ball_by_ball_mean <- tibble(
    ball = c(1, 2, 3, 4, 5),
    `mean scored` = c(7/3, 4/3, 10/3, 2/2, 6/1),
    `mean SR` = round(c(7/3, 4/3, 10/3, 2/2, 6/1) * 100, 2)
  )
  
  model <- lm(ball_by_ball_mean[["mean SR"]] ~ ball_by_ball_mean[["ball"]])
  
  # expected result and run function
  expected_result <- tibble(
    Innings = 3,
    `Balls per boundary` = round(12/4, 2),
    `Dot ball %` = round(3/12 * 100, 2),
    `Mean runs scored` = round(mean(player_innings[["total scored"]]), 2),
    `Median runs scored` = median(player_innings[["total scored"]]),
    `Mean strike rate` = round(sum(player_innings[["total scored"]])/sum(player_innings[["balls faced"]])*100, 2),
    `Median strike rate` = median(player_innings[["SR"]]),
    `Mean balls faced` = round(mean(player_innings[["balls faced"]]), 2),
    `Median balls faced` = median(player_innings[["balls faced"]]),
    Acceleration = unname(model[["coefficients"]][2]),
    BASRA = `Mean runs scored` + `Mean strike rate`
  )
  
  player_summary_table <- career_summary_table(ball_by_ball_data, player_innings, model)
  
  expect_equal(player_summary_table, expected_result)
})


