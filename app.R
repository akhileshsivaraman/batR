#---- batR ----
# a tool to analyse batting statistics
library(shiny)
library(bslib)
library(cricketdata)
library(tidyverse)
library(DT)



#---- outside UI and server ----
# vector of tournaments
tournaments <- cricsheet_codes
mens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "it20s", "ipl", "apl", "bbl", "bpl", "cpl", "ctc", "ilt", "ipl", "ipt", "lpl", "msl", "ntb", "psl", "sat", "sma")) |>
  pull(code)

#---- functions ---
# get ball by ball data of a male player. Generates a list where each item is an innings
find_bbb_male <- function(player_name){
  innings_tibble <- tibble()
  
  for (i in mens_t20_tournaments){
    x <- fetch_cricsheet(type = "bbb",
                         gender = "male",
                         competition = i) |>
      filter(striker == player_name) |>
      mutate(tournament = paste0(i))
    
    innings_tibble <- rbind(innings_tibble, x)
  }
  
  y <- split(innings_tibble, f = innings_tibble$match_id)
  
  return(y)
}

# function to filter out wides and put each ball in a column
condense <- function(x){
  y <- x |>
    select(match_id, runs_off_bat, wides, tournament) |>
    filter(is.na(wides)) |>
    select(!wides) |>
    mutate(ball = row_number()) |>
    pivot_wider(names_from = ball, values_from = runs_off_bat)
  return(y)
}

# apply condense to items in a list and join the resulting tibbles to create ball by ball data
career_bbb <- function(innings_list){
  x <- lapply(innings_list, condense)
  y <- reduce(.x = x, .f = full_join)
  return(y)
}

# calculate mean runs scored and mean SR by ball faced
career_mean_bbb <- function(ball_by_ball_data){
  x <- ball_by_ball_data |>
    select(!match_id & !tournament) |>
    summarise(across(where(is.numeric), mean, na.rm = T)) |>
    mutate(across(where(is.numeric), ~round(.x, 2))) |>
    pivot_longer(cols = everything(), names_to = "ball", values_to = "mean scored") |>
    mutate(ball = as.numeric(ball),
           "mean SR" = `mean scored`*100)
  
  return(x)
}

# calculate mean runs scored and mean SR by ball faced broken down by tournament
tournament_mean_bbb <- function(ball_by_ball_data){
  x <- ball_by_ball_data |>
    group_by(tournament) |>
    select(!match_id) |>
    summarise(across(where(is.numeric), mean, na.rm = T)) |>
    mutate(across(where(is.numeric), ~round(.x, 2))) |>
    pivot_longer(cols = 2:(ncol(ball_by_ball_data)-1), names_to = "ball", values_to = "mean scored") |>
    mutate(ball = as.numeric(ball),
           "mean SR" = `mean scored`*100)
  
  return(x)
}

# create a tibble of all the innings a player has played
innings_table <- function(ball_by_ball_data){
  x <- ball_by_ball_data |>
    pivot_longer(cols = 3:ncol(ball_by_ball_data), names_to = "ball", values_to = "runs per ball") |>
    drop_na(`runs per ball`) |>
    mutate(match_id = as_factor(match_id)) |>
    group_by(match_id) |>
    summarise("total scored" = round(sum(`runs per ball`), 2))
  
  y <- ball_by_ball_data |>
    pivot_longer(cols = 3:ncol(ball_by_ball_data), names_to = "ball", values_to = "runs scored") |>
    drop_na(`runs scored`) |>
    count(as_factor(match_id)) |>
    rename(match_id = `as_factor(match_id)`,
           "balls faced" = n) |>
    left_join(x) |>
    mutate(SR = round(`total scored`/`balls faced`*100, 2))
  
  return(y)
}

# calculate balls per boundary
balls_per_boundary <- function(ball_by_ball_data){
  balls <- ball_by_ball_data[3:ncol(ball_by_ball_data)]
  balls_faced <- sum(balls, na.rm = T)
  boundary_count <- sum(balls == 4 | balls == 6, na.rm = T)
  balls_per_boundary_rate <- round(balls_faced/boundary_count, 2)
  return(balls_per_boundary_rate)
}

# calculate dot ball %
dot_ball_percentage <- function(ball_by_ball_data){
  balls <- ball_by_ball_data[3:ncol(ball_by_ball_data)]
  dot_ball_count <- sum(balls == 0, na.rm = T)
  balls_faced <- sum(balls, na.rm = T)
  dot_ball_percent <- round(dot_ball_count/balls_faced*100, 2)
  return(dot_ball_percent)
}




#---- UI ----
ui <- fluidPage(
  
  theme = bs_theme(version = 5, bg = "#FBFFF1", fg = "#000000", primary = "#090C9B", secondary = "#3066BE", font_scale = 0.8),
  
  titlePanel("batR"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel(title = "batR",
             fluidRow(
               column(12,
                      textInput(inputId = "player_selected",
                                label = "Enter the name of a player",
                                value = "MEK Hussey"),
                      helpText("The name of the player must be entered in the form found on scorecards with the player's full initials followed by their surname. E.g. CH Gayle. You can find other examples on ESPN Cricinfo scorecards.")
               )
             ),
             
             fluidRow(
               column(12,
                      h3("Summary statistics"),
                      tableOutput(outputId = "summary_table")
               )
             ),
             
             fluidRow(
               column(12,
                      h3("Career ball by ball summary"),
                      dataTableOutput(outputId = "ball_by_ball_table")
               )
             ),
             
             fluidRow(
               column(12,
                      h3("Career strike rate by ball"),
                      plotOutput(outputId = "ball_by_ball_SR_plot"),
                      p("The horizontal line is the player's career strike rate and the black line is a model of how their typical innings progresses (steeper black line = greater acceleration). Where the two lines meet indicates how many balls it takes the player to reach their mean strike rate.")
               )
             ),
             
             fluidRow(
               column(12,
                      h3("Career strike rate by ball broken down by tournament"),
                      plotOutput("tournament_ball_by_ball_SR_plot"),
                      p("The horizontal line is the player's career strike rate and the other lines are models of how their typical innings progresses at each tournament.")
               )
             )
            ),
    
    tabPanel(title = "About",
             h3("About"),
             tags$hr(),
             p("batR is a tool to analyse batting data from T20 matches."),
             br(),
             p("All you need to do is enter the name of a player in the form initials + surname. The app then calculates summary statistics for the player and plots how that player tends to perform each ball. Currently, the app only supports finding data for men's players"),
             br(),
             h5("Statistics calculated"),
             tags$hr(),
             tags$ul(
               tags$li("Ball per boundary: quite simply in T20 cricket, the team that hits the most boundaries tends to win."),
               tags$line("Dot ball percentage: with only 120 balls to score from, it's critical that batters score from as many balls as possible, even if it's just 1 run. Losing sides often have high dot ball percentages."),
               tags$li("Mean runs: we don't calculate the traditional batting average here. We calculate the mean number of runs scored per innings. The batting average we generally use in cricket is a measure of how many runs a batter scores per dismissal but when it's a new game, you don't get to carry over your score from the last game if you were not out so for many players the idea of how many runs they score per game is inflated."),
               tags$li("Median runs: mean runs scored tells us an average of how many runs a player score per innings but it's not a perfect metric. The number of runs scored by players fluctuates so some averages are propped up by a handful of brilliant innings, which masks a lot of low scores. The median gives us a better idea of how consistent the batter is by telling us what 50% of the scores are greater (or less) than."),
               tags$li("Mean SR: the usual career strike rate of player."),
               tags$li("Median SR: just as with runs, strike rates can be inflated by a few extraordinary knocks. Knowing what the median is gives us a better idea of what a player's strike rate is when they get out. Batter's with low medians could be chewing up a lot of balls and scoring not many."),
               tags$li("Mean balls faced: the number of balls faced a player faces per innings. If a batter tends to face few deliveries, they will ideally have high strike rates."),
               tags$li("Median balls faced: another measure for the number of balls a player faces per innings.")
             ),
             br(),
             h5("Coming soon"),
             tags$hr(),
             tags$ul(
               tags$li("Adding support for women's cricket stats (unfortunately, due to the way the functions that support this app work, only men's stats are currently supported)"),
               tags$li("Interactive plots"),
               tags$li("Selecting a player without needing their initials"),
               tags$li("Side-by-side player comparisons")
             )
            )
    
    )
  
  
  
  
)


#---- server ----
server <- function(input, output){
  
  #---- reactives ----
  #--- get ball by ball data for the player selected ---
  innings_list <- reactive({
    find_bbb_male(input$player_selected)
  })
  
  #--- create ball by ball data ---
  ball_by_ball_data <- reactive({
    career_bbb(innings_list())
  })
  
  #--- mean runs scored and mean SR by ball faced ---
  ball_by_ball_mean <- reactive({
    career_mean_bbb(ball_by_ball_data())
  })
  
  #--- linear model for strike rate by ball number ---
  model <- reactive({
    lm(ball_by_ball_mean()$`mean SR` ~ ball_by_ball_mean()$ball)
  })
  
  #--- mean runs scored and mean SR by ball faced at each tournament ---
  tournament_ball_by_ball_mean <- reactive({
    tournament_mean_bbb(ball_by_ball_data())
  })
  
  #--- create tibble of all innings played by the player ---
  player_innings <- reactive({
    innings_table(ball_by_ball_data())
  })
  
  #--- calculate mean runs scored ---
  mean_runs_scored <- reactive({
    round(sum(player_innings()$`total scored`)/sum(player_innings()$`balls faced`)*100, 2)
  })
  
  #--- calculate median runs scored ---
  median_runs_scored <- reactive({
    median(player_innings()$`total scored`)
  })
  
  #--- calculate mean SR ---
  mean_SR <- reactive({
    round(mean(player_innings()$SR), 2)
  })
  
  #--- calculate median SR ---
  median_SR <- reactive({
    median(player_innings()$SR)
  })
  
  #--- calculate mean balls faced ---
  mean_balls_faced <- reactive({
    round(mean(player_innings()$`balls faced`), 2)
  })
  
  # calculate median balls faced ---
  median_balls_faced <- reactive({
    median(player_innings()$`balls faced`)
  })
  
  #--- calculate ball per boundary ---
  boundary_rate <- reactive({
    balls_per_boundary(ball_by_ball_data())
  })
  
  # calculate dot ball percentage
  dbp <- reactive({
    dot_ball_percentage(ball_by_ball_data())
  })
  
  
  #---- outputs ----
  #--- summary table ---
  output$summary_table <- renderTable({
    cbind(boundary_rate(), dbp(), mean_runs_scored(), median_runs_scored(), mean_SR(), median_SR(), mean_balls_faced(), median_balls_faced()) |>
      `colnames<-`(c("Ball per boundary",
                     "Dot ball %",
                     "Mean runs scored",
                     "Median runs scored",
                     "Mean strike rate",
                     "Median strike rate",
                     "Mean balls faced",
                     "Median balls faced"))
  })
  
  
  #--- ball by ball table ---
  output$ball_by_ball_table <- renderDataTable({
    ball_by_ball_mean()
  }, options = list(pageLength = 15))
  
  
  #--- ball by ball SR plot ---
  output$ball_by_ball_SR_plot <- renderPlot({
    ggplot(ball_by_ball_mean()) +
      geom_point(aes(ball, `mean SR`)) +
      geom_hline(yintercept = mean_SR(), linetype = "dashed", alpha = 0.5) +
      scale_x_continuous(n.breaks = nrow(ball_by_ball_mean())/5) +
      geom_abline(slope = model()$coefficients[2], intercept = model()$coefficients[1], alpha = 0.5) +
      theme(plot.background = element_rect(fill = "#FBFFF1"),
            panel.background = element_rect(fill = "#FBFFF1"),
            axis.text = element_text(colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"))
  })
  
  
  #--- ball by ball SR split by tournament ---
  output$tournament_ball_by_ball_SR_plot <- renderPlot({
    ggplot(tournament_ball_by_ball_mean()) +
      geom_point(aes(ball, `mean SR`, colour = tournament), size = 0.7) +
      geom_smooth(aes(ball, `mean SR`, colour = tournament), method = "lm", se = F) +
      geom_hline(yintercept = mean_SR(), linetype = "dashed", alpha = 0.5) +
      scale_x_continuous(n.breaks = nrow(ball_by_ball_mean())/5) +
      geom_abline(slope = model()$coefficients[2], intercept = model()$coefficients[1], alpha = 0.5)+
      theme(plot.background = element_rect(fill = "#FBFFF1"),
            panel.background = element_rect(fill = "#FBFFF1"),
            axis.text = element_text(colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"))
  })
}


#---- app ----
shinyApp(ui, server)