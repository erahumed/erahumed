#' @title `r erahumed_docs("layers", "hbd", "title")`
#' @name hbd
#'
#' @family simulation layers
#'
#' @description `r erahumed_docs("layers", "hbd", "description")`
#'
#' @return An object of class \link{erahumed_simulation}.
#'
#' @details No details
#'
#' @export
setup_hbd <- function(simulation)
{
  tryCatch(
    {
      assert_erahumed_simulation(simulation)
    },
    error = function(e) {
      class(e) <- c("validate_hbd_params_error", class(e))
      stop(e)
    })

  setup_layer(layer = "hbd")
}

compute_hbd <- function(simulation)
{
  output <- data.frame()

  validate_hbd_output(output)

  simulation [["hbd"]] [["output"]] <- output

  return(simulation)
}

validate_hbd_output <- function(output) {
  assert_data.frame(output)
}
