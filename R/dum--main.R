# Dummy simulation layer definition, only used for testing purposes

dum <- function(simulation)
  get_simulation_layer(simulation, "dum")

compute_dum <- function(simulation, numeric_param = 0)
  compute_layer(simulation, "dum", numeric_param)

compute_dum_output <- function(simulation, numeric_param)
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
