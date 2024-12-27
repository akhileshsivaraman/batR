#----- ball_by_ball_analysis.R -----

#---- functions ----


#---- ball_by_ball_analysis_UI ----
ball_by_ball_analysis_UI <- function(id){
  tagList(
    fluidRow(
      column(
        12,
        h3("Ball by ball analysis"),
        tags$hr()
      )
    ),
    
    fluidRow(
      column(
        4,
        h4("Career ball by ball analysis"),
        dataTableOutput(NS(id, "ball_by_ball_table"))
      ),
      column(
        8,
        h4("Career strike rate by ball"),
        plotOutput(NS(id, "ball_by_ball_SR_plot")),
        textOutput(NS(id, "balls_to_mean_SR")),
        p("The horizontal line is the player's career strike rate and the black line is a model of how their typical innings progresses (steeper black line = greater acceleration). Where the two lines meet indicates how many balls it takes the player to reach their mean strike rate.")
      )
    ),
    
    fluidRow(
      column(
        12,
        h4("Strike rate by ball across tournaments"),
        plotlyOutput(NS(id, "tournament_ball_by_ball_SR_plot")),
        p("The horizontal line is the player's career strike rate and the other lines are models of how their typical innings progresses at each tournament.")
      )
    )
  )
}


#---- ball_by_ball_analysis_server ----
ball_by_ball_analysis_server <- function(id, ball_by_ball_mean, model, tournament_ball_by_ball_mean, selected_player, player_summary_table){
  moduleServer(id, function(input, output, session){
    
    # ball by ball table
    output$ball_by_ball_table <- renderDataTable({
      ball_by_ball_mean()
    }, options = list(pageLength = 10), rownames = F)
    
    
    # get mean SR
    mean_SR <- observe({player_summary_table[["Mean strike rate"]][[1]]})
    
    
    # ball by ball SR plot
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
    
    # calculate balls taken to reach mean SR
    balls_to_reach_mean_SR <- reactive({
      round((mean_SR()-model()$coefficients[1])/model()$coefficients[2], 1)
    })
    
    output$balls_to_mean_SR <- renderText({
      paste("On average", selected_player$player_name, "takes", balls_to_reach_mean_SR(), "balls to reach their mean SR.")
    })
    
    
    # ball by ball SR split by tournament
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
    
  })
}


#---- ball_by_ball_analysis_app ----
ball_by_ball_analysis_app <- function(ball_by_ball_mean, model, tournament_ball_by_ball_mean, selected_player, player_summary_table){
  
  ui <- page_fluid(
    ball_by_ball_analysis_UI("ball_by_ball_analysis")
  )
  
  server <- function(input, output, session){
   ball_by_ball_analysis_server("ball_by_ball_analysis", ball_by_ball_mean, model, tournament_ball_by_ball_mean, selected_player, player_summary_table) 
  }
  
  shinyApp(ui, server)
}
# ball_by_ball_analysis_app(ball_by_ball_mean, model, tournament_ball_by_ball_mean, selected_player, player_summary_table))