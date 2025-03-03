#' @title `r erahumed_docs("layers", "inp", "title")`
#' @name inp
#'
#' @family simulation layers
#'
#' @description `r erahumed_docs("layers", "inp", "description")`
#'
#' @param simulation `[`\link{erahumed_simulation}`]` \cr
#' The simulation object being modified.
#' @param outflows_df `r erahumed_param_roxy("outflows_df", "inp")`
#' @param weather_df `r erahumed_param_roxy("weather_df", "inp")`
#' @param variety_prop `r erahumed_param_roxy("variety_prop", "inp")`
#' @param seed `r erahumed_param_roxy("seed", "inp")`
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
#' @noRd
setup_inp <- function(simulation, outflows_df, weather_df, variety_prop, seed)
{
  tryCatch(
    {
      assert_erahumed_simulation(simulation)

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

  setup_layer(layer = "inp")
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

