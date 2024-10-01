#' @title CT: Chemical Transport
#' @name ct
#'
#' @family model components
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' This model component computes the evolution of chemicals applied to rice
#' paddy clusters, based on the previously computed simulations for
#' hydrological balance and chemicals application.
#' The result is a set of time series of concentrations, one for each
#' applied chemical.
#'
#' This modeling layer requires the \link{ca} component of the model to be
#' pre-computed.
#'
#' @param model An object of class \link{erahumed_model}, with a pre-computed
#' \link{ca} component (*i.e.* such that `ca(model)` is not `NULL`).
#'
#' @return Objects of class \link{erahumed_model} and `erahumed_ct`, for
#' `compute_ct()` and `ct()` respectively.
#'
#' @details
#' TBD.
#' @rdname ct
#' @export
ct <- function(model)
  get_model_component(model, "ct")

#' @rdname ct
#' @export
compute_ct <- function(model)
{
  compute_component(model, "ct")
}



compute_ct_argcheck <- function()
{
  tryCatch({
    TRUE
  },
  error = function(e) {
    class(e) <- c("compute_ct_argcheck_error", class(e))
    stop(e)
  })
}



compute_ct_output <- function(model)
{
  return(data.frame())
}



ct_validate_output <- assert_data.frame
