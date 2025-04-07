#----- Update batR data -----
# last updated: 26th March 2025
library(cricketdata)
library(dplyr)
library(duckdb)

tournaments <- cricsheet_codes
con <- DBI::dbConnect(duckdb(), dbdir = "data/t20_batting_data.duckdb")

#---- save men's ball-by-ball data ----
mens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "it20s", "ipl", "apl", "bbl", "bpl", "cpl", "ctc", "ilt", "ipl", "ipt", "lpl", "msl", "ntb", "psl", "sat", "sma", "ssm")) |>
  pull(code)

male_data <- tibble()
for(i in mens_t20_tournaments){
  x <- fetch_cricsheet(type = "bbb",
                       gender = "male",
                       competition = i) |>
    mutate(tournament = paste0(i))
  
  male_data <- rbind(male_data, x) 
}

DBI::dbWriteTable(con, "mens_ball_by_ball_data", male_data, overwrite = TRUE)


#---- save women's ball-by-ball data ----
womens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "blz", "cec", "frb", "wbb", "wcl", "wsl", "wtc")) |>
  pull(code)

female_data <- tibble()
for(i in womens_t20_tournaments){
  x <- fetch_cricsheet(type = "bbb",
                       gender = "female",
                       competition = i) |>
    mutate(tournament = paste0(i))
  
  female_data <- rbind(female_data, x)
}

DBI::dbWriteTable(con, "womens_ball_by_ball_data", female_data, overwrite = TRUE)

DBI::dbDisconnect(con)