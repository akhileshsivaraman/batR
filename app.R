#---- batR ----
# a tool to analyse batting statistics
library(shiny)
library(bslib)
library(cricketdata)
library(tidyverse)
library(DT)
library(shinyjs)
library(plotly)


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


#--- functions ---
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

# spider plot
source("functions/spider_plot.R")


#---- UI ----
ui <- fluidPage(
  
  useShinyjs(),
  
  theme = bs_theme(version = 5, bg = "#FBFFF1", fg = "#000000", primary = "#1A281F", secondary = "#FFA630", font_scale = 0.8),
  
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
                      uiOutput(outputId = "innings_warning"),
                      br()
                )
             ),
             
             fluidRow(
               column(12,
                      h3("Summary statistics"),
                      plotOutput(outputId = "summary_plot"),
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
                      plotlyOutput("tournament_ball_by_ball_SR_plot"),
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
               tags$li("Median balls faced: another measure for the number of balls a player faces per innings."),
               tags$li("Acceleration: how quickly a batter's strike rate increases as the innings goes on"),
               tags$li("BASRA: stands for batting average and strike rate aggregate. As runs aren't the only currency in T20 cricket, BASRA helps us compare two players by taking into account strike rates too.")
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
  
  #--- number of innings played ---
  innings_n <- reactive({
    nrow(ball_by_ball_data())
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
  
  #--- BASRA ---
  basra <- reactive({
    mean_runs_scored() + mean_SR()
  })
  
  #--- summary table ---
  player_summary_table <- reactive({
    tibble(innings_n(), boundary_rate(), dbp(), 
          mean_runs_scored(), median_runs_scored(), 
          mean_SR(), median_SR(), 
          mean_balls_faced(), median_balls_faced(),
          unname(model()$coefficients[2]), basra())  |>
      `colnames<-`(c("Innings",
                     "Balls per boundary",
                     "Dot ball %",
                     "Mean runs scored",
                     "Median runs scored",
                     "Mean strike rate",
                     "Median strike rate",
                     "Mean balls faced",
                     "Median balls faced",
                     "Acceleration",
                     "BASRA"))
  })
  
  #---- outputs ----
  output$a <- renderText({
    paste("the player has played", innings_n(), "innings")
  })
  
  #--- summary table ---
  output$summary_table <- renderTable({
    player_summary_table()
  })
  
  
  #--- summary plot ---
  output$summary_plot <- renderPlot({
    spider_plot(player_summary_table(), player_innings())
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
  output$tournament_ball_by_ball_SR_plot <- renderPlotly({
    ggplotly(ggplot(tournament_ball_by_ball_mean()) +
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
            legend.position = "top"))
  })
}


#---- app ----
shinyApp(ui, server)