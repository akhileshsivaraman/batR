#' Pivot ball by ball data for all innings played by a player
#' 
#' For each innings in a list, filter out wide deliveries and pivot the tibble into a time series-like table tracking the innings. The tables are then joined together into one large tibble that is returned.
#' 
#' @param innings_list - list: a list of tibbles where each tibble represents an innings played by a player

career_bbb <- function(innings_list, with_progress = FALSE){
  
  # initialise progress object or not
  if(with_progress){
    progress <- Progress$new(
      min = 0,
      max = (length(innings_list) + 5)
    )
    progress$set(value = 0,
                 message = "Preparing ball by ball data for analysis",
                 detail = "Filtering data"
    )
  } else {
    progress <- NULL
  }
  
  # filter out wides and pivot
  x <- lapply(innings_list, condense_career, with_progress = with_progress, progress_object = progress)
  
  # update progress object
  if(with_progress){
    progress$inc(amount = 1,
                 detail = "Joining data"
    )
  }
  # join all the tibbles together
  y <- reduce(.x = x, .f = full_join)
  
  # final update and close progress object
  if(with_progress){
    progress$inc(amount = 4)
    progress$close()
  }
  
  return(y)
}


#' `condense_career` - a helper function to filter out wides and put each ball in a column
condense_career <- function(x, with_progress = FALSE, progress_object = NULL){
  
  if(with_progress){
    progress_object$inc(amount = 1)
  }
  
  y <- x |>
    select(match_id, runs_off_bat, wides, tournament) |>
    filter(is.na(wides)) |>
    select(!wides) |>
    mutate(ball = row_number()) |>
    pivot_wider(names_from = ball, values_from = runs_off_bat)
  return(y)
}
