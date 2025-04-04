#' Pivot and group by phase ball by ball data for all innings by a player
#' 
#' For each innings in a list, assign an innings phase to a ball then filter out wide deliveries and pivot the tibble into a time series-like table tracking the innings. The tables are then joined together into one large tibble that is returned.
#' 
#' @param innings_list - list: a list of tibbles where each tibble represents an innings played by a player


phase_bbb <- function(innings_list){
  x <- lapply(innings_list, condense_by_phase)
  y <- reduce(.x = x, .f = full_join)
  return(y)
}

#' `condense_by_phase` - a helper function to assign the phase then filter out wides and put each ball in a column
condense_by_phase <- function(x){
  y <- x |>
    mutate(phase = case_when(over < 6 ~ "powerplay",
                             over < 15 ~ "middle",
                             over < 21 ~ "death")) |>
    select(match_id, runs_off_bat, wides, phase) |>
    filter(is.na(wides)) |>
    select(!wides) |>
    group_by(phase) |>
    mutate(ball = row_number()) |>
    pivot_wider(names_from = ball, values_from = runs_off_bat)
  return(y)
}