# workings for an analysis by year range

con <- dbConnect(duckdb(), "data/t20_batting_data.duckdb")

jadeja <- find_bbb(
  "RA Jadeja",
  "male",
  con
)

bbb_data <- career_bbb(jadeja)

# use the jadeja list to get match ids and start_date then join onto all_innings
year_data <- lapply(jadeja, \(x){
  y <- x |>
    select(match_id, start_date) |>
    mutate(year = year(start_date)) |>
    select(!start_date) |>
    distinct()
})
year_data_df <- reduce(year_data, full_join) |>
  mutate(match_id = as_factor(match_id))

all_innings <- innings_table(bbb_data)

filtered_innings <- all_innings |>
  left_join(year_data_df) |>
  filter(year > 2019)

mean_sr <- mean(filtered_innings[["SR"]])
mean_total_scored <- mean(filtered_innings[["total scored"]])

# this lets you get the overall stats for the time period so will need to add the year to the bbb data