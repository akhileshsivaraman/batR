#---- batR ----
# a tool to analyse batting statistics

#---- libraries ----
library(shiny)
library(bslib)
library(cricketdata)
library(tidyverse)
library(DT)
library(shinyjs)
library(plotly)


#---- load data ----
mens_t20_data <- read_rds("data/mens_ball_by_ball_data.rds")
womens_t20_data <- read_rds("data/womens_ball_by_ball_data.rds")

# vector of tournaments for men and women
tournaments <- cricsheet_codes
mens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "it20s", "ipl", "apl", "bbl", "bpl", "cpl", "ctc", "ilt", "ipl", "ipt", "lpl", "msl", "ntb", "psl", "sat", "sma", "ssm")) |>
  pull(code)

womens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "blz", "cec", "frb", "wbb", "wcl", "wsl", "wtc")) |>
  pull(code)


#---- functions ----
# get ball by ball data of a player. Generates a list where each item is an innings
source("functions/find_bbb.R")

# two functions: one to filter out wides and put each ball in a column for an innings, one to apply that function to a list of innings and join the tibbles to create ball by ball data
source("functions/career_bbb.R")

# calculate mean runs scored and mean SR by ball faced
source("functions/career_mean_bbb.R")

# calculate mean runs scored and mean SR by ball faced broken down by tournament
source("functions/tournament_mean_bbb.R")

# create a tibble of all the innings a player has played
source("functions/innings_table.R")

# calculate balls per boundary
source("functions/balls_per_boundary.R")

# calculate dot ball %
source("functions/dot_ball_percentage.R")

# career summary table
source("functions/career_summary_table.R")

# spider plot
source("functions/spider_plot.R")

# metrics by tournament
source("functions/metrics_by_tournament.R")

# ball by ball data by phase of a player
source("functions/phase_bbb.R")

# metrics by phase
source("functions/metrics_by_phase.R")

# spider plot by phase
source("functions/spider_plot_by_phase.R")

# find initials
source("functions/find_initials.R")


#---- modules ----
source("modules/about.R")
source("modules/ball_by_ball_analysis.R")
source("modules/career_summary.R")
source("modules/find_player.R")
source("modules/select_player.R")
source("modules/stats_breakdown.R")


#---- UI ----
ui <- page_navbar(
  title = "batR",
  
  useShinyjs(),
  
  # theme = bs_theme(version = 5, bg = "#FBFFF1", fg = "#000000", primary = "#1A281F", secondary = "#FFA630", font_scale = 0.8),
  
  # titlePanel("batR"),
  
  nav_panel(
    title = "batR",
    select_player_UI("select_player"), 
    career_summary_UI("career_summary"),
    stats_breakdown_UI("stats_breakdown"),
    ball_by_ball_analysis_UI("ball_by_ball_analysis")
  ),
  
  nav_panel(
    title = "Find Player",
    find_player_UI("find_player")
  ),
  
  nav_panel(
    title = "About",
    about_UI("about")
  )
)


#---- server ----
server <- function(input, output, session){
  
  # get innings list for selected player ----
  selected_player <- select_player_server("select_player")
  
  
  # computations ----
  # create ball by ball data
  ball_by_ball_data <- reactive({
    if(length(selected_player$innings_list()) > 0){
      career_bbb(selected_player$innings_list())
    }
  })
  
  # mean runs scored and mean SR by ball faced
  ball_by_ball_mean <- reactive({
    career_mean_bbb(ball_by_ball_data())
  })
  
  # linear model for strike rate by ball number
  model <- reactive({
    lm(ball_by_ball_mean()$`mean SR` ~ ball_by_ball_mean()$ball)
  })
  
  # mean runs scored and mean SR by ball faced at each tournament
  tournament_ball_by_ball_mean <- reactive({
    tournament_mean_bbb(ball_by_ball_data())
  })
  
  # create tibble of all innings played by the player
  player_innings <- reactive({
    innings_table(ball_by_ball_data())
  })
  
  # career summary table
  player_summary_table <- reactive({
    career_summary_table(ball_by_ball_data(), player_innings(), model())
  })
  
  
  # render career summary stats ----
  career_summary_server("career_summary", player_summary_table, player_innings)
  
  
  # stats breakdowns ----
  stats_breakdown_server("stats_breakdown", ball_by_ball_data, player_innings, selected_player$innings_list)
  
  
  # ball_by_ball_analysis ----
  ball_by_ball_analysis_server("ball_by_ball_analysis", ball_by_ball_mean, model, tournament_ball_by_ball_mean, selected_player, player_summary_table)
  
  
  # find_player ----
  find_player_server("find_player")
  
  
  # about ----
  about_server("about")
}


#---- app ----
shinyApp(ui, server, session)