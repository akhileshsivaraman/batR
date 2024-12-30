#' Calculate balls per boundary
#' 
#' Calculate the number of balls a player takes to hit a boundary
#' 
#' @param ball_by_ball_data - tibble: a tibble of ball by ball data produced by `career_bbb()`

balls_per_boundary <- function(ball_by_ball_data){
  balls <- ball_by_ball_data[3:ncol(ball_by_ball_data)]
  balls_faced <- sum(balls, na.rm = T)
  boundary_count <- sum(balls == 4 | balls == 6, na.rm = T)
  balls_per_boundary_rate <- round(balls_faced/boundary_count, 2)
  return(balls_per_boundary_rate)
}
