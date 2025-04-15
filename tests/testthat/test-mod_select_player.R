test_that("select_player_server returns a list and a character vector when find_bbb is provided correct inputs", {
  test_con <- dbConnect(duckdb(), test_path("fixtures", "test_data.duckdb"))
  expected_innings_list <- find_bbb(
    "S Mandhana",
    "female",
    test_con
  )
  
  testServer(select_player_server, args = list(con = test_con), {
    
    # set player_selected & male_or_female then click find_data
    session$setInputs(
      player_selected = "S Mandhana",
      male_or_female = "female"
    )
    session$setInputs(find_data = 0)
    
    selected_player <- session$getReturned()
    expect_type(selected_player$player_name(), "character")
    expect_type(selected_player$innings_list(), "list")
  })
})

test_that("innings_warning renders a warning to the UI when the incorrect gender or an incorrect name is supplied", {
  test_con <- dbConnect(duckdb(), test_path("fixtures", "test_data.duckdb"))
  
  testServer(select_player_server, args = list(con = test_con), {
    # test for incorrect gender
    session$setInputs(
      player_selected = "S Mandhana",
      male_or_female = "male"
    )
    session$setInputs(find_data = 0)
    
    # output$innings_warning produces a list of two. The first item is the HTML so we can check that the warning written into the app is produced when we expect it to be produced
    warning_in_html <- grepl(
      pattern = "Warning: please check that you have selected the correct gender for the player or check the spelling of their name", 
      x = output$innings_warning$html
    )
    expect_true(warning_in_html)
    
    
    # test for incorrect name
    session$setInputs(
      player_selected = "A Sivaraman",
      male_or_female = "male"
    )
    session$setInputs(find_data = 0)
    warning_in_html <- grepl(
      pattern = "Warning: please check that you have selected the correct gender for the player or check the spelling of their name", 
      x = output$innings_warning$html
    )
    expect_true(warning_in_html)
  })
})
