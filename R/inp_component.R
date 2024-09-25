new_inp_component <- function(output, params) {
  res <- new_model_component(output,
                             params,
                             output_template = inp_output_template(),
                             validate_params = inp_validate_params
                             )
  class(res) <- c("erahumed_inp", class(res))
  return(res)
}


inp_validate_params <- function(params) {
  # Skipped
  return(TRUE)
}

inp_output_template <- function() {
  # Skipped
  return(NULL)
}
