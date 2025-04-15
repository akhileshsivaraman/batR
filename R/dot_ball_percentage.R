#' Calculate dot ball percentage
#' 
#' Calulate the percentage of balls faced by a player that are dot balls
#' 
#' @param ball_by_ball_data - tibble: a tibble of ball by ball data produced by `career_bbb()`

dot_ball_percentage <- function(ball_by_ball_data){
  balls <- ball_by_ball_data[3:ncol(ball_by_ball_data)]
  dot_ball_count <- sum(balls == 0, na.rm = T)
  balls_faced <- sum(!is.na(balls), na.rm = T)
  dot_ball_percent <- round(dot_ball_count/balls_faced*100, 2)
  return(dot_ball_percent)
}
