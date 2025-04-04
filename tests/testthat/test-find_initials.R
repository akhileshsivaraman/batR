test_that("find_initials returns a dataframe of 3 columns containing the player of interest", {
  player_to_find <- "Ellyse Perry"
  df <- find_initials(player_to_find)
  
  expect_true(is.data.frame(df))
  expect_length(df, 3)
  expect_true(grepl(player_to_find, df[["Name"]])) # test relies on the name of the player being an exact match
})

