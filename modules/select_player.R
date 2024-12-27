#----- select_player.R -----

#---- functions ----
source("functions/find_bbb.R")

#---- select_player_UI ----
select_player_UI <- function(id){
  tagList(
    fluidRow(
      column(
        12,
        br(),
        fluidRow(
          column(
            width = 4,
            tags$div(
              textInput(
                NS(id, "player_selected"),
                label = "Search for a player",
                placeholder = "E.g. MEK Hussey"
              )
            )
          ),
          column(
            width = 4,
            tags$div(
              radioButtons(
                NS(id, "male_or_female"),
                label = "Male or Female Player?",
                choices = c("male", "female"),
                selected = "male",
                inline = TRUE
              )
            )
          )
        ),
        
        helpText("The name of the player must be entered in the form of their full initials + surname. E.g. `CH Gayle`. You can search for a player of interest's name in this form on the Find Player page."),
        br(),
        
        tags$div(
          actionButton(
            NS(id, "find_data"), 
            label = "Find data"
          ),
          hidden(
            tags$div(
              NS(id, "loading_spinner"),
              icon("spinner"),
              class = "fa-spin",
              style = "display: inline-block")
          ),
          style = "display:inline"
        ),
        br(),
        
        uiOutput(NS(id, "innings_warning"))
      )
    )
  )
}

#---- select_player_server ----
select_player_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    # when the find data button is clicked, give the user feedback
    observeEvent(input$find_data, {
      disable(id = "find_data")
      show(id = "loading_spinner")
      delay(5000, {
        enable(id = "find_data")
        hide(id = "loading_spinner")
      })
    })
    
    # get the data for the requested player
    innings_list <- eventReactive(input$find_data, {
      find_bbb(player_name = input$player_selected, 
               gender = input$male_or_female, 
               mens_t20_data = mens_t20_data, 
               womens_t20_data = womens_t20_data
      )
    })
    
    # render a warning when innings_list is length 0
    output$innings_warning <- renderUI({
      if(length(innings_list()) < 1){
        div(icon("exclamation-circle"), "Warning: please check that you have selected the correct gender for the player or check the spelling of their name", style = "color:red")
      } else{
        div("", style = "color:red")
      }
    })
    
    
    
    list(
      innings_list = innings_list,
      player_name = reactive({input$player_selected})
    )
  })
}


#---- select_player_app ----
select_player_app <- function(){
  ui <- page_fluid(
    select_player_UI("select_player")
  )
  
  server <- function(input, output, session){
    select_player_server("select_player")
  }
  
  shinyApp(ui, server)
}

# select_player_app()