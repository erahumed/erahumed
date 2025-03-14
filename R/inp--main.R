compute_inp <- function(simulation)
{
  outflows_df <- get_input(simulation, "outflows_df")
  weather_df <- get_input(simulation, "weather_df")
  date_start <- get_input(simulation, "date_start")
  date_end <- get_input(simulation, "date_end")

  simulation [["outputs"]] [["inp"]] <-
    merge(outflows_df, weather_df, by = "date", sort = TRUE) |>
    (\(.) .[date_start <= .$date & .$date <= date_end, ])()

  return(simulation)
}

