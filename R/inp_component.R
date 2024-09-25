new_inp_component <- function(output, params) {
  res <- new_model_component(output,
                             params,
                             validate_output = inp_validate_output,
                             validate_params = inp_validate_params
                             )
  class(res) <- c("erahumed_inp", class(res))
  return(res)
}


inp_validate_output <- assert_data.frame
inp_validate_params <- assert_list
