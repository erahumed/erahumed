new_ca_component <- function(output, params) {
  res <- new_model_component(output,
                             params,
                             validate_output = ca_validate_output,
                             validate_params = ca_validate_params
  )
  class(res) <- c("erahumed_ca", class(res))
  return(res)
}

ca_validate_params <- assert_list

ca_validate_output <- assert_data.frame


#' @export
print.erahumed_ca <- function(x, ..., max = 100) {
  cat(bold("An object of class 'ca'."))

  min_date <- format(as.Date(min(x$output$date)))
  max_date <- format(as.Date(max(x$output$date)))
  cat("\nData from:", min_date, "to:", max_date, "\n\n")

  print.data.frame(x$output, max = max)
}

#' @export
summary.erahumed_ca <- function(object, ..., max = 100) {
  print(object, max = max)
}
