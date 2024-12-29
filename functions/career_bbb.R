#' Pivot ball by ball data for all innings played by a player
#' 
#' For each innings in a list, filter out wide deliveries and pivot the tibble into a time series-like table tracking the innings. The tables are then joined together into one large tibble that is returned.
#' 
#' @param innings_list - list: a list of tibbles where each tibble represents an innings played by a player

career_bbb <- function(innings_list){
  x <- lapply(innings_list, condense_career)
  y <- reduce(.x = x, .f = full_join)
  return(y)
}

#' `condense_career` - a helper function to filter out wides and put each ball in a column
condense_career <- function(x){
  y <- x |>
    select(match_id, runs_off_bat, wides, tournament) |>
    filter(is.na(wides)) |>
    select(!wides) |>
    mutate(ball = row_number()) |>
    pivot_wider(names_from = ball, values_from = runs_off_bat)
  return(y)
}
