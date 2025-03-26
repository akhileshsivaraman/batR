#----- select_player.R -----

#---- functions ----
source("functions/find_bbb.R")

#---- select_player_UI ----
select_player_UI <- function(id){
  tagList(
    card(
      fill = FALSE,
      card_header(h3("Select Player"), class = "bg-primary"),
      fluidRow(
        column(
          12,
          fluidRow(
            column(
              width = 4,
              tags$div(
                textInput(
                  NS(id, "player_selected"),
                  label = "Enter player name",
                  placeholder = "E.g. JE Root"
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
          
          helpText("The name of the player must be entered in the form of their full initials + surname. E.g. `CH Gayle`. You can search for a player of interest's name in this format on the Find Player page."),
          br(),
          br(),
          
          actionButton(
            NS(id, "find_data"), 
            label = "Analyse player stats"
          ),
          br(),
          
          uiOutput(NS(id, "innings_warning"))
        )
      )
    )
  )
}


#---- select_player_server ----
select_player_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    
    # get the data for the requested player
    innings_list <- eventReactive(input$find_data, {
      find_bbb(player_name = input$player_selected, 
               gender = input$male_or_female,
               con = con,
               with_progress = TRUE
      )
    })
    
    
    # render a warning when innings_list is length 0
    output$innings_warning <- renderUI({
      if(length(innings_list()) < 1){
        div(icon("exclamation-circle"), "Warning: please check that you have selected the correct gender for the player or check the spelling of their name", style = "color:red")
      } else{
        div(style = "height: 5px")
      }
    })
    
    
    list(
      innings_list = innings_list,
      player_name = reactive({input$player_selected})
    )
  })
}


#---- select_player_app ----
select_player_app <- function(mens_t20_data, womens_t20_data){
  ui <- page_fluid(
    select_player_UI("select_player")
  )
  
  server <- function(input, output, session){
    select_player_server("select_player", mens_t20_data, womens_t20_data)
  }
  
  shinyApp(ui, server)
}

# select_player_app()