#' Career summary table
#' 
#' Calculate summary stats over the course of a player's career.
#' 
#' This function calculates:
#' * the number of innings played
#' * boundary rate
#' * dot ball percentage
#' * mean & median runs scored per innings
#' * mean & median strike rate in an innings
#' * median balls faced
#' * acceleration
#' * BASRA
#' returning them in a tibble
#' 
#' @param ball_by_ball_data - tibble: a tibble of ball by ball data produced by `career_bbb()`
#' @param player_innings - tibble: a tibble summarising all innings a player has played over their career produced by `innings_table()`
#' @param model - lm object: a linear model describing the relationship between balls faced and SR

career_summary_table <- function(ball_by_ball_data, player_innings, model) {
  innings_n <- nrow(ball_by_ball_data)
  boundary_rate <- balls_per_boundary(ball_by_ball_data)
  dbp <- dot_ball_percentage(ball_by_ball_data)
  mean_runs_scored <- round(mean(player_innings$`total scored`), 2)
  median_runs_scored <- median(player_innings$`total scored`)
  mean_SR <- round(sum(player_innings$`total scored`)/sum(player_innings$`balls faced`)*100, 2)
  median_SR <- median(player_innings$SR)
  mean_balls_faced <- round(mean(player_innings$`balls faced`), 2)
  median_balls_faced <- median(player_innings$`balls faced`)
  acceleration <- unname(model$coefficients[2])
  basra <- mean_runs_scored + mean_SR
  
  player_summary_table <- tibble(innings_n,
                                 boundary_rate,
                                 dbp,
                                 mean_runs_scored,
                                 median_runs_scored,
                                 mean_SR,
                                 median_SR,
                                 mean_balls_faced,
                                 median_balls_faced,
                                 acceleration,
                                 basra) |>
    `colnames<-`(c("Innings",
                   "Balls per boundary",
                   "Dot ball %",
                   "Mean runs scored",
                   "Median runs scored",
                   "Mean strike rate",
                   "Median strike rate",
                   "Mean balls faced",
                   "Median balls faced",
                   "Acceleration",
                   "BASRA"))
  
  return(player_summary_table)
}
