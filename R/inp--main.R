compute_inp <- function(simulation)
{
  outflows_df <- get_input(simulation, "outflows_df")
  weather_df <- get_input(simulation, "weather_df")

  simulation [["outputs"]] [["inp"]] <-
    merge(outflows_df, weather_df, by = "date", sort = TRUE)

  return(simulation)
}

