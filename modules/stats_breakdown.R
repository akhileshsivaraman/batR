#----- stats_breakdown.R -----

#---- functions ----
source("functions/metrics_by_tournament.R")
source("functions/phase_bbb.R")
source("functions/metrics_by_phase.R")
source("functions/spider_plot_by_phase.R")


#---- stats_breakdown_UI ----
stats_breakdown_UI <- function(id){
  tagList(
    card(
      card_header(h3("Career Stats Breakdowns"), class = "bg-primary"),
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
  )
}


#---- stats_breakdown_server ----
stats_breakdown_server <- function(id, ball_by_ball_data, player_innings, selected_player){
  moduleServer(id, function(input, output, session){
    
    #--- tournament breakdown ---
    # cache the result too
    
    observe({
      if(input$stats_breakdown_option == "Tournament"){
        
        withProgress(
          min = 0,
          max = 1,
          message = paste("Computing stats by tournament for", selected_player$player_name()),
          expr = {
            # compute tournament summary table
            list_metrics_by_tournament_summary <- reactive({
              ball_by_ball_by_tournament <- split(ball_by_ball_data(), ball_by_ball_data()$tournament)
              tournament_summary <- lapply(ball_by_ball_by_tournament, metrics_by_tournament)
            })
            
            incProgress(amount = 0.4)
            
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
            incProgress(amount = 0.3)
            output$tournament_summary_table <- renderTable({
              metrics_by_tournament_summary_table()
            })
            
            
            # display tournament summary plot
            incProgress(
              amount = 0.2,
              message = "Creating plots"
            )
            output$tournament_summary_plots <- renderPlot({
              par(mfrow = c(ceiling(length(list_metrics_by_tournament_summary())/3), 3))
              for (i in 1:length(list_metrics_by_tournament_summary())){
                spider_plot(list_metrics_by_tournament_summary()[[i]], player_innings(), title = names(list_metrics_by_tournament_summary()[i]))
              }
            })
            
            incProgress(amount = 0.1)
          }
        )
      }
    })
    
    
    #--- phase breakdown ---
    # cache result
    
    observe({
      if(input$stats_breakdown_option == "Phase"){
        
        withProgress(
          min = 0,
          max = 1,
          message = paste("Computing stats by phase for", selected_player$player_name()),
          expr = {
            # compute phase summary table
            player_bbb_by_phase <- reactive({
              phase_bbb(selected_player$innings_list())
            })
            
            incProgress(amount = 0.4)
            
            metrics_by_phase_summary_table <- reactive({
              metrics_by_phase_summary <- metrics_by_phase(player_bbb_by_phase())
            })
            
            
            # display phase summary plot
            incProgress(
              amount = 0.4,
              message = "Creating plots"
            )
            
            output$phase_summary_plots <- renderPlot({
              spider_plot_by_phase(player_bbb_by_phase(), metrics_by_phase_summary_table())
            })
            
            
            # display phase summary table
            output$phase_summary_table <- renderTable({
              metrics_by_phase_summary_table()
            })
            
            
            incProgress(amount = 0.2)
          }
        )
      }
    })
    
    
  })
}


#---- stats_breakdown_app ----
stats_breakdown_app <- function(ball_by_ball_data, player_innings, selected_player){
  
  ui <- page_fluid(
    stats_breakdown_UI("stats_breakdown")
  )
  
  server <- function(input, output, session){
    stats_breakdown_server("stats_breakdown", ball_by_ball_data, player_innings, selected_player)
  }
  
  shinyApp(ui, server)
}

# stats_breakdown_app(ball_by_ball_data, player_innings, selected_player)