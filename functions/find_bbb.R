#' Get ball by ball data for a player
#' 
#' Filters data to every ball faced by a player in T20 cricket then splits the large table into a list of tables whereby every table is an innings played by the player
#' 
#' @param player_name - string: a cricket player's name
#' @param gender - string: male or female
#' @param mens_t20_data - tibble: T20 data from men's matches
#' @param womens_t20_data - tibble: T20 data from women's matches
#' @param with_progress - boolean: whether or not to include a progress bar. Should be FALSE outside of an interactive context
#' @import shiny

find_bbb <- function(player_name, gender, mens_t20_data, womens_t20_data, with_progress = FALSE){
  
  # initialise progress bar
  if(with_progress){
    progress <- Progress$new(
      min = 0,
      max = 1
    )
    progress$set(value = 0,
                 message = "Fetching player data"
    )
  }
  
  # get data
  if(gender == "male"){
    x <- mens_t20_data |>
      filter(striker == player_name)
  } else if(gender == "female"){
    x <- womens_t20_data |>
      filter(striker == player_name)
  }
  
  # split data by match id
  y <- split(x, f = x$match_id)
  
  # update progress and close
  if(with_progress){
    progress$inc(amount = 1)
    progress$close()
  }
  
  return(y)
}
