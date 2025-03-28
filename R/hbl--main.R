compute_hbl <- function(simulation)
{
  storage_curve_slope_m2 <- get_input(simulation, "storage_curve_slope_m2")
  storage_curve_intercept_m3 <- get_input(simulation, "storage_curve_intercept_m3")
  petp_surface_m2 <- get_input(simulation, "petp_surface_m2")
  inp_df <- get_output(simulation, "inp")

  output <-
    .hbl(
      element_id = "lake",
      level = inp_df$level,
      precipitation_mm = inp_df$precipitation_mm,
      evapotranspiration_mm = inp_df$evapotranspiration_mm,
      outflows = inp_df[, grepl("^outflow_", colnames(inp_df))],
      date = inp_df$date,
      storage_curve_slope_m2 = storage_curve_slope_m2,
      storage_curve_intercept_m3 = storage_curve_intercept_m3,
      petp_surface_m2 = petp_surface_m2
    )

  validate_hbl_output(output)

  simulation [["outputs"]] [["hbl"]] <- output
  return(simulation)
}



validate_hbl_output <- function(output) {
  assert_data.frame(output,
                    template = data.frame(level = numeric(),
                                          precipitation_mm = numeric(),
                                          evapotranspiration_mm = numeric(),
                                          volume = numeric(),
                                          inflow_total = numeric(),
                                          outflow_total = numeric(),
                                          outflow_recirculation = numeric(),
                                          residence_time_days = numeric()
                    )
  )
}

