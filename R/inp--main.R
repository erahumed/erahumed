#' @title INP: Input data
#' @name inp
#'
#' @family simulation layers
#'
#' @description
#' This simulation layer has the only purpose to collect the observational (or,
#' potentially, synthetic) data used by the algorithms of the ERAHUMED decision
#' support system.
#'
#' This is the first layer of the simulation chain and does not have
#' any upstream dependence.
#'
#' @param simulation An object of class \link{erahumed_simulation}.
#' @param outflows_df `r erahumed_param_roxy("outflows_df")`
#' @param weather_df `r erahumed_param_roxy("weather_df")`
#' @param variety_prop `r erahumed_param_roxy("variety_prop")`
#' @param seed `r erahumed_param_roxy("seed")`
#'
#' @return An object of class \link{erahumed_simulation}.
#'
#' @details
#'
#' Both input `data.frame`s should have a `date` column (of class \link{Date}),
#' and the corresponding date domain should be an interval (*i.e.* no missing
#' data between the maximum and minimum of `date` is allowed). The subsequent
#' simulation will only be performed on the largest date interval in which both
#' hydrological and weather input data is available.
#'
#' @export
setup_inp <- function(simulation,
                      outflows_df = erahumed::albufera_outflows,
                      weather_df = erahumed::albufera_weather,
                      variety_prop = c("J.Sendra" = 0.8,
                                       "Bomba" = 0.1,
                                       "Clearfield" = 0.1),
                      seed = 840
                      )
{
  setup_layer(simulation = simulation,
              layer = "inp",
              outflows_df = outflows_df,
              weather_df = weather_df,
              variety_prop = variety_prop,
              seed = seed,
              validate_params = validate_inp_params
              )
}

compute_inp <- function(simulation)
{
  outflows_df <- get_layer_parameters(simulation, "inp")[["outflows_df"]]
  weather_df <- get_layer_parameters(simulation, "inp")[["weather_df"]]
  variety_prop <- get_layer_parameters(simulation, "inp")[["variety_prop"]]
  seed <- get_layer_parameters(simulation, "inp")[["seed"]]

  cv_map <- withr::with_seed(seed, generate_clusters_variety(variety_prop))
  simulation [["inp"]] [["aux"]] <- list( cluster_variety_map = cv_map )

  simulation [["inp"]] [["output"]] <-
    merge(outflows_df, weather_df, by = "date", sort = TRUE)

  return(simulation)
}

validate_inp_params <- function(outflows_df, weather_df, variety_prop, seed)
{
  tryCatch(
    {
      outflow_required_cols <- c("date",
                                 "level",
                                 "is_imputed_level",
                                 "is_imputed_outflow")
      assert_data.frame(
        outflows_df,
        template = erahumed::albufera_outflows[, outflow_required_cols]
      )
      assert_data.frame(weather_df, template = erahumed::albufera_weather)

      # Check that consecutive date differences are all equal to one
      if (any(diff(outflows_df$date) != 1)) {
        stop("Invalid 'date' domain in 'outflows_df' (not an interval).")
      }
      if (any(diff(weather_df$date) != 1)) {
        stop("Invalid 'date' domain in 'weather_df' (not an interval)." )
      }

      assert_positive_vector(variety_prop)
      if (length(variety_prop) != 3) {
        stop("'variety_prop' must have length 3.")
      }

    },
    error = function(e) {
      class(e) <- c("validate_inp_params_error", class(e))
      stop(e)
    })
}
