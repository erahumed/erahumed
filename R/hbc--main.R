compute_hbc <- function(simulation)
{
  management_df <- get_input(simulation, "management_df")
  clusters_df <- albufera_clusters
  ideal_flow_rate_cm <- get_input(simulation, "ideal_flow_rate_cm")
  height_thresh_cm <- get_input(simulation, "height_thresh_cm")
  cv_map <- get_etc(simulation, "cluster_variety_map")
  seed <- get_input(simulation, "seed")

  withr::with_seed(seed, {
    .hbc_args <- .hbc_data_prep(simulation = simulation,
                                management_df = management_df,
                                clusters_df = clusters_df,
                                ideal_flow_rate_cm = ideal_flow_rate_cm,
                                height_thresh_cm = height_thresh_cm,
                                cv_map = cv_map)

    output <- do.call(.hbc, .hbc_args)
    })

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
                                            cluster_id = character(),
                                            ditch = character(),
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
  assert_character(output$cluster_id)
  assert_character(output$ditch)
  assert_positive_vector(output$ideal_outflow_cm, tol = 1e-6)
  assert_positive_vector(output$plan_delay, tol = 1e-6)
  assert_integer_vector(output$plan_delay)
  assert_positive_vector(output$height_eod_cm, tol = 1e-6)
}
