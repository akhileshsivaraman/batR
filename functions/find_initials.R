#' Find initials for a player of interest
#' 
#' Performs matching to a supplied player name to identify potential record matches in the form of initials + surname.
#' 
#' @param player_to_find - string: name of a player

find_initials <- function(player_to_find){
  d <- find_player_id(player_to_find) |>
    select(c("Name", "Country", "Played"))
}
