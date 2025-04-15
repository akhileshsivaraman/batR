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

test_that("innings_warning renders a warning to the UI when innings_list is less than 0", {
  
})