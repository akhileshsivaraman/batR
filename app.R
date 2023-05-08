#---- batR ----
# a tool to analyse batting statistics
library(shiny)
library(bslib)
library(cricketdata)
library(tidyverse)
library(DT)
library(shinyjs)


#---- outside UI and server ----
# load data
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


#---- functions ---
# get ball by ball data of a player. Generates a list where each item is an innings
find_bbb <- function(player_name, gender){
  if(gender == "male"){
    x <- mens_t20_data |>
      filter(striker == player_name)
  } else if(gender == "female"){
    x <- womens_t20_data |>
      filter(striker == player_name)
  }
  
  y <- split(x, f = x$match_id)
  
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
  
  useShinyjs(),
  
  theme = bs_theme(version = 5, bg = "#FBFFF1", fg = "#000000", primary = "#090C9B", secondary = "#3066BE", font_scale = 0.8),
  
  titlePanel("batR"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel(title = "batR",
             fluidRow(
               column(12,
                      br(),
                      fluidRow(
                        column(width = 4,
                               tags$div(textInput(inputId = "player_selected",
                                                  label = "Enter the name of a player",
                                                  value = "MEK Hussey"), 
                                        style = "display:inline-block")),
                        column(width = 4,
                               tags$div(radioButtons(inputId = "male_or_female", 
                                                     label = "Male or Female Player?", 
                                                     choices = c("male", "female"), 
                                                     selected = "male",
                                                     inline = TRUE), 
                                        style = "display:inline-block"))
                        ),
                      br(),
                      helpText("The name of the player must be entered in the form found on scorecards with the player's full initials followed by their surname. E.g. CH Gayle. You can find other examples on ESPN Cricinfo scorecards."),
                      br(),
                      br(),
                      tags$div(actionButton(inputId = "action", 
                                            label = "Find data"),
                               hidden(tags$div(id = "loading_spinner", 
                                               icon("spinner"), 
                                               class = "fa-spin", 
                                               style = "display: inline-block")),
                               style = "display:inline"),
                      br(),
                      uiOutput(outputId = "innings_warning")
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
                      textOutput(outputId = "balls_to_mean_SR"),
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
             p("All you need to do is enter the name of a player in the form initials + surname. The app then calculates summary statistics for the player and plots how that player tends to perform each ball. The data used are up to date as of 4th May 2023."),
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
  # when the find data button is clicked, give the user feedback
  observeEvent(input$action, {
    disable(id = "action")
    show(id = "loading_spinner")
    delay(5000, {
      enable(id = "action")
      hide(id = "loading_spinner")
      })
  })
  
  # get the data
  innings_list <- eventReactive(input$action, {
    find_bbb(player_name = input$player_selected, gender = input$male_or_female)
  })
  
  # render a warning when innings_list is length 0
  output$innings_warning <- renderUI({
    if(length(innings_list()) < 1){
      div(icon("exclamation-circle"), "Warning: please check that you have selected the correct gender for the player or check the spelling of their name", style = "color:red")
    } else{
      div("", style = "color:red")
    }
  })
  
  #--- create ball by ball data ---
  ball_by_ball_data <- reactive({
    if(length(innings_list()) > 0){
      career_bbb(innings_list())
      }
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
    round(mean(player_innings()$`total scored`), 2)
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
  
  #--- calculate median balls faced ---
  median_balls_faced <- reactive({
    median(player_innings()$`balls faced`)
  })
  
  #--- calculate ball per boundary ---
  boundary_rate <- reactive({
    balls_per_boundary(ball_by_ball_data())
  })
  
  #--- calculate dot ball percentage ---
  dbp <- reactive({
    dot_ball_percentage(ball_by_ball_data())
  })
  
  #--- calculate balls taken to reach mean SR ---
  balls_to_reach_mean_SR <- reactive({
    round((mean_SR()-model()$coefficients[1])/model()$coefficients[2], 1)
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
  }, options = list(pageLength = 10), rownames = F)
  
  
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
  
  
  #--- balls to reach mean SR ---
  output$balls_to_mean_SR <- renderText({
    paste("On average", input$player_selected, "takes", balls_to_reach_mean_SR(), "balls to reach their mean SR.")
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
            axis.ticks = element_line(colour = "black"),
            legend.background = element_blank(),
            legend.position = "top")
  })
}


#---- app ----
shinyApp(ui, server)