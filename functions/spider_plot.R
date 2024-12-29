#' Create a spider plot
#' 
#' Create a spider plot displaying a player's career stats:
#' * balls per boundary
#' * dot ball percentage
#' * median runs scored
#' * median strike rate
#' * acceleration
#' * BASRA
#' 
#' @param player_summary_table - tibble: a career summary table produced by `career_summary_table()`
#' @param player_innings - tibble: a tibble summarising all innings a player has played over their career produced by `innings_table()`
#' @param title - string: a title to display with the plot
#' 
#' @import tidyverse
#' @import fmsb

spider_plot <- function(player_summary_table, player_innings, title = NULL) {
  player_summary_table <- player_summary_table |>
    select(c("Balls per boundary", "Dot ball %", "Median runs scored", "Median strike rate", "Acceleration", "BASRA"))
  
  max_runs <- max(player_innings$`total scored`)
  max_SR <- max(player_innings$SR)
  max_BASRA <- player_innings |>
    mutate(BASRA = `total scored` + SR) |>
    pull(BASRA) |>
    max()
  
  max_values <- c(1, 0, max_runs, max_SR, 4, max_BASRA)
  min_values <- c(20, 100, 0, 0, 0, 0)
  data <- rbind(max_values, min_values, player_summary_table)
  
  par(mar = c(3, 3, 2, 1), bg = "#FBFFF1")
  radarchart(df = data, axistype = 2,
             seg = 5, cglty = 2,
             cglcol = "lightgrey",
             axislabcol = "#6A8E7F",
             pfcol = alpha("#6A8E7F", 0.5),
             pcol = "#6A8E7F",
             vlabels = c("Balls per boundary", "Dot ball %", "Median runs scored", "Median SR", "Acceleration", "BASRA"), 
             vlcex = 1.2, title = title)
}
