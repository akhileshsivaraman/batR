#' Find initials for a player of interest
#' 
#' Performs matching to a supplied player name to identify potential record matches in the form of initials + surname.
#' 
#' @param player_to_find - string: name of a player

find_initials <- function(player_to_find, with_progress = FALSE){
  
  if(with_progress){
    progress <- Progress$new(
      min = 0,
      max = 1
    )
    progress$set(
      value = 0.1,
      message = "Finding players"
    )
    
    progress$inc(amount = 0.1)
  }
  
  d <- find_player_id(player_to_find) |>
    select(c("Name", "Country", "Played"))
  
  if(with_progress){
    progress$set(value = 1)
    progress$close()
  }
  
  return(d)
}
