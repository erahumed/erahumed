# Dummy model layer definition, only used for testing purposes

dum <- function(model)
  get_simulation_layer(model, "dum")

compute_dum <- function(model, numeric_param = 0)
  compute_layer(model, "dum", numeric_param)

compute_dum_output <- function(model, numeric_param)
  data.frame()

compute_dum_argcheck <- function(numeric_param)
{
  tryCatch(assert_numeric_vector(numeric_param),
           error = function(e) {
             class(e) <- c("compute_dum_argcheck_error", class(e))
             stop(e)
             }
           )
}

dum_validate_output <- function(output)
  assert_data.frame(output)
