compute_hbc <- function(simulation)
{
  cluster_map <- get_input(simulation, "cluster_map")
  ideal_flow_rate_cm <- get_input(simulation, "ideal_flow_rate_cm")
  height_thresh_cm <- get_input(simulation, "height_thresh_cm")
  seed <- get_input(simulation, "seed")

  withr::with_seed(seed, {
    .hbc_args <- .hbc_data_prep(simulation = simulation,
                                cluster_map = cluster_map,
                                ideal_flow_rate_cm = ideal_flow_rate_cm,
                                height_thresh_cm = height_thresh_cm
                                )

    output <- do.call(.hbc, .hbc_args)
    })

  names(output)[names(output) == "cluster_id"] <- "element_id"
  names(output)[names(output) == "ditch"] <- "ditch_element_id"


  validate_hbc_output(output)

  simulation [["outputs"]] [["hbc"]] <- output

  return(simulation)
}



validate_hbc_output <- function(output) {
  assert_data.frame(output,
                    template =   data.frame(ideal_height_eod_cm = numeric(),
                                            height_eod_cm = numeric(),
                                            ideal_irrigation = logical(),
                                            ideal_draining = logical(),
                                            irrigation = logical(),
                                            draining = logical(),
                                            petp_cm = numeric(),
                                            area_m2 = numeric(),
                                            capacity_m3_s = numeric(),
                                            date = as.Date(character()),
                                            element_id = character(),
                                            ditch_element_id = character(),
                                            ideal_inflow_cm = numeric(),
                                            ideal_outflow_cm = numeric(),
                                            inflow_cm = numeric(),
                                            outflow_cm = numeric(),
                                            inflow_m3_s = numeric(),
                                            outflow_m3_s = numeric(),
                                            plan_delay = numeric()
                    )
  )

  assert_positive_vector(output$ideal_height_eod_cm, tol = 1e-6)
  assert_positive_vector(output$area_m2, tol = 1e-6)
  assert_positive_vector(output$capacity_m3_s, tol = 1e-6)
  assert_date(output$date)
  assert_character(output$element_id)
  assert_character(output$ditch)
  assert_positive_vector(output$ideal_outflow_cm, tol = 1e-6)
  assert_positive_vector(output$plan_delay, tol = 1e-6)
  assert_integer_vector(output$plan_delay)
  assert_positive_vector(output$height_eod_cm, tol = 1e-6)
}
