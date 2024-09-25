new_hba_component <- function(output, params) {
  res <- new_model_component(output,
                             params,
                             validate_output = hba_validate_output,
                             validate_params = hba_validate_params
  )
  class(res) <- c("erahumed_hba", class(res))
  return(res)
}

hba_validate_params <- assert_list

hba_validate_output <- function(output) {
  assert_data.frame(output,
                    template = data.frame(level = numeric(),
                                          rain_mm = numeric(),
                                          evapotranspiration_mm = numeric(),
                                          volume = numeric(),
                                          inflow_total = numeric(),
                                          outflow_total = numeric(),
                                          outflow_extra = numeric(),
                                          residence_time_days = numeric()
                                          )
                    )
}


#' @export
print.erahumed_hba <- function(x, ..., max = 100) {
  cat(bold("An object of class 'hba'."))

  min_date <- format(as.Date(min(x$output$date)))
  max_date <- format(as.Date(max(x$output$date)))
  cat("\nData from:", min_date, "to:", max_date, "\n\n")

  print.data.frame(x$output, max = max)
}

#' @export
summary.erahumed_hba <- function(object, ..., max = 100) {
  print(object, max = max)
}
