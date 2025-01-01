#----- find_player.R -----

#---- functions ----


#---- find_player_UI ----
find_player_UI <- function(id){
  tagList(
    h3("Find Player"),
    sidebarLayout(
      sidebarPanel(
        p("On this page, you can search for the initials of a player whose stats you would like to analyse. You can type in the full name of the player or just their surname and the table opposite will give you their name in the form of initials + surname, which you will need to use batR."),
        textInput(
          NS(id, "player_to_find"), 
          label = "Search for a player's initials",
          placeholder = "E.g. \"Perry\", \"E Perry\" or \"Ellyse Perry\""
        ),
        actionButton(
          NS(id, "find_player"),
          label = "Find player"
        ),
        hidden(
          tags$div(
            id = NS(id, "loading_spinner_2"),
            icon("spinner"), 
            class = "fa-spin", 
            style = "display: inline-block"
          )
        )
      ),
      
      mainPanel(
        h4("Search results"),
        tableOutput(NS(id, "players_found_table")),
        uiOutput(NS(id, "players_found_warning"))
      )
    )
  )
}


#---- find_player_server ----
find_player_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    # search for players that match the search string
    players_found <- eventReactive(input$find_player, {
      find_initials(input$player_to_find, with_progress = TRUE)
    })
    
    # render a warning when players_found is length 0
    output$players_found_warning <- renderUI({
      if(length(players_found()) < 1){
        div(icon("exclamation-circle"), "Unfortunately, we cannot find a player with that name.", style = "color:red")
      } else{
        div("", style = "color:red")
      }
    })
    
    # render the table
    output$players_found_table <- renderTable({
      players_found()
    })
  })
}

#---- find_player_app ----
find_player_app <- function(){
  
  ui <- page_fluid(
    find_player_UI("find_player")
  )
  
  server <- function(input, output, session){
    find_player_server("find_player")
  }
  
  shinyApp(ui, server)
}
# find_player_app()