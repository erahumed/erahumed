new_hbp_component <- function(output, params) {
  res <- new_model_component(output,
                             params,
                             validate_output = hbp_validate_output,
                             validate_params = hbp_validate_params
                             )
  class(res) <- c("erahumed_hbp", class(res))
  return(res)
}

hbp_validate_params <- function(params) {
  # Skipped
  return(TRUE)
}

hbp_validate_output <- function(output) {
  assert_data.frame(output,
                    template =   data.frame(ideal_height_cm = numeric(),
                                            real_height_cm = numeric(),
                                            ideal_irrigation = logical(),
                                            ideal_draining = logical(),
                                            real_irrigation = logical(),
                                            real_draining = logical(),
                                            petp_cm = numeric(),
                                            area_m2 = numeric(),
                                            capacity_m3_s = numeric(),
                                            date = as.Date(character()),
                                            cluster_id = character(),
                                            ditch = character(),
                                            ideal_inflow_cm = numeric(),
                                            ideal_outflow_cm = numeric(),
                                            real_inflow_cm = numeric(),
                                            real_outflow_cm = numeric(),
                                            real_inflow_m3_s = numeric(),
                                            real_outflow_m3_s = numeric(),
                                            plan_delay = numeric()
                                            )
                    )

  assert_positive_vector(output$ideal_height_cm, tol = 1e-6)
  assert_positive_vector(output$area_m2, tol = 1e-6)
  assert_positive_vector(output$capacity_m3_s, tol = 1e-6)
  assert_date(output$date)
  assert_character(output$cluster_id)
  assert_character(output$ditch)
  assert_positive_vector(output$ideal_outflow_cm, tol = 1e-6)
  assert_positive_vector(output$plan_delay, tol = 1e-6)
  assert_integer_vector(output$plan_delay)
  assert_positive_vector(output$real_height_cm, tol = 1e-6)
}


#' @export
print.erahumed_hbp <- function(x, ..., max = 100) {
  cat(bold("An object of class 'hbp'."))

  min_date <- format(as.Date(min(x$output$date)))
  max_date <- format(as.Date(max(x$output$date)))
  cat("\nData from:", min_date, "to:", max_date, "\n\n")

  print.data.frame(x$output, max = max)
}

#' @export
summary.erahumed_hbp <- function(object, ..., max = 100) {
  print(object, max = max)
}
