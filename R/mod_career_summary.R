#----- career_summary.R ----

#---- functions ----
# source("functions/spider_plot.R")

#---- career_summary_UI ----
career_summary_UI <- function(id){
  tagList(
    card(
      fill = FALSE,
      card_header(h3("Career Summary Stats"), class = "bg-primary"),
      fluidRow(
        column(
          12,
          plotOutput(NS(id, "career_summary_plot")),
          tableOutput(NS(id, "career_summary_table")),
        )
      )
    )
  )
}


#---- career_summary_server ----
career_summary_server <- function(id, player_summary_table, player_innings){
  moduleServer(id, function(input, output, session){
    
    # create spider plot
    output$career_summary_plot <- renderPlot({
      spider_plot(player_summary_table(), player_innings())
    })
    
    # create career summary table
    output$career_summary_table <- renderTable({
      player_summary_table()
    })
  })
}


#---- career_summary_app ----
career_summary_app <- function(player_summary_table, player_innings){
  ui <- page_fluid(
    career_summary_UI("career_summary")
  )
  
  server <- function(input, output, session){
    career_summary_server("career_summary", player_summary_table, player_innings)
  }
  
  shinyApp(ui, server)
}
# career_summary_app(player_summary_table, player_innings)