#----- about.R -----

#---- about_UI ----
about_UI <- function(id){
  tagList(
    h3("About"),
    tags$hr(),
    p("batR is a tool to analyse batting data from T20 matches."),
    p("All you need to do is enter the name of a player in the form initials + surname. The app then calculates summary statistics for the player and plots how that player tends to perform each ball. The data used are up to date as of 4th May 2023."),
    br(),
    h5("Statistics calculated"),
    tags$hr(),
    tags$ul(
      tags$li("Ball per boundary: quite simply in T20 cricket, the team that hits the most boundaries tends to win."),
      tags$li("Dot ball percentage: with only 120 balls to score from, it's critical that batters score from as many balls as possible, even if it's just 1 run. Losing sides often have high dot ball percentages."),
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
      tags$li("Further breakdowns of a player's stats"),
      tags$li("Side-by-side player comparisons")
    )
  )
}

#---- abount_server ----
about_server <- function(id){
  moduleServer(id, function(input, output, session){
    
  })
}

#---- about_app ----
about_app <- function(){
  
  ui <- page_fluid(
    about_UI("about")
  )
  
  server <- function(input, output, session){
    about_server("about")
  }
  
  shinyApp(ui, server)
}
# about_app()