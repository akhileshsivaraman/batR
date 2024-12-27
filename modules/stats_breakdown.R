#----- stats_breakdown.R -----

#---- functions ----
source("functions/metrics_by_tournament.R")
source("functions/phase_bbb.R")
source("functions/metrics_by_phase.R")
source("functions/spider_plot_by_phase.R")


#---- stats_breakdown_UI ----
stats_breakdown_UI <- function(id){
  tagList(
    fluidRow(
      column(
        12,
        h4("Breakdown player statistics by:"),
        tags$div(
          selectInput(
            NS(id, "stats_breakdown_option"),
            label = "Options",
            choices = c("None selected", "Tournament", "Phase"),
            selected = "None selected",
            multiple = F
          ),
          hidden(
            tags$div(
              id = NS(id, "loading_spinner_breakdown_option"),
              icon("spinner"), 
              class = "fa-spin", 
              style = "display: inline-block"
            )
          ),
          style = "display:inline"
        ),
        conditionalPanel(
          condition = "input.stats_breakdown_option == 'Tournament'",
          ns = NS(id),
          h5("Tournament"),
          plotOutput(NS(id, "tournament_summary_plots")), # spider plots by tournament
          tableOutput(NS(id, "tournament_summary_table")) # metrics by tournament
        ),
        conditionalPanel(
          condition = "input.stats_breakdown_option == 'Phase'",
          ns = NS(id),
          h5("Phase"),
          plotOutput(NS(id, "phase_summary_plots")),
          tableOutput(NS(id, "phase_summary_table")),
        )
      )
    )
  )
}


#---- stats_breakdown_server ----
stats_breakdown_server <- function(id, ball_by_ball_data, player_innings, innings_list){
  moduleServer(id, function(input, output, session){
    
    # give the user feedback when they select an option for breaking down stats
    observeEvent(input$stats_breakdown_option, {
      disable(id = "stats_breakdown_option")
      show(id = "loading_spinner_breakdown_option")
      delay(2500, {
        enable(id = "stats_breakdown_option")
        hide(id = "loading_spinner_breakdown_option")
      })
    })
    
    
    #--- tournament breakdown ---
    # compute tournament summary table
    list_metrics_by_tournament_summary <- reactive({
      ball_by_ball_by_tournament <- split(ball_by_ball_data(), ball_by_ball_data()$tournament)
      tournament_summary <- lapply(ball_by_ball_by_tournament, metrics_by_tournament)
    })
    
    metrics_by_tournament_summary_table <- reactive({
      y <- tibble()
      for (i in 1:length(list_metrics_by_tournament_summary())){
        x <- list_metrics_by_tournament_summary()[[i]] |>
          mutate(tournament = names(list_metrics_by_tournament_summary()[i]),
                 .before = 1)
        y <- rbind(y, x)
      }
      return(y)
    })
    
    # display tournament summary table
    output$tournament_summary_table <- renderTable({
      metrics_by_tournament_summary_table()
    })
    
    # display tournament summary plot
    output$tournament_summary_plots <- renderPlot({
      par(mfrow = c(ceiling(length(list_metrics_by_tournament_summary())/3), 3))
      for (i in 1:length(list_metrics_by_tournament_summary())){
        spider_plot(list_metrics_by_tournament_summary()[[i]], player_innings(), title = names(list_metrics_by_tournament_summary()[i]))
      }
    })
    
    
    #--- phase breakdown ---
    # compute phase summary table
    player_bbb_by_phase <- reactive({
      phase_bbb(innings_list())
    })
    
    metrics_by_phase_summary_table <- reactive({
      metrics_by_phase_summary <- metrics_by_phase(player_bbb_by_phase())
    })
    
    # display phase summary plot
    output$phase_summary_plots <- renderPlot({
      spider_plot_by_phase(player_bbb_by_phase(), metrics_by_phase_summary_table())
    })
    
    # display phase summary table
    output$phase_summary_table <- renderTable({
      metrics_by_phase_summary_table()
    })
    
  })
}


#---- stats_breakdown_app ----
stats_breakdown_app <- function(ball_by_ball_data, player_innings, innings_list){
  
  ui <- page_fluid(
    stats_breakdown_UI("stats_breakdown")
  )
  
  server <- function(input, output, session){
    stats_breakdown_server("stats_breakdown", ball_by_ball_data, player_innings, innings_list)
  }
  
  shinyApp(ui, server)
}

# stats_breakdown_app(ball_by_ball_data, player_innings, innings_list)