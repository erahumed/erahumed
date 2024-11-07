#' @title HBP: Hydrological Balance of rice Paddies
#' @name hbp
#'
#' @family simulation layers
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' This simulation layer performs a simulation of hydrological balance
#' daily data for the rice paddies of the Albufera National Park, that provides
#' a synthetic dataset of daily inflows, outflows and average water levels for
#' each rice paddy.
#'
#' The inputs to this simulation are:
#' 1. The previously calculated hydrological balance for the Albufera lake.
#' 1. A geographical subdivision of the Albufera National Park into "clusters"
#' of rice paddies, of a definite rice variety, and whose waters flow into the
#' same ditch.
#' 1. An yearly ideal schedule for irrigation and draining, with corresponding
#' expected water levels of a cluster for each day of the year.
#'
#' @param simulation An object of class \link{erahumed_simulation}.
#' @param clusters_df A `data.frame` that contains the definition of rice paddy
#' clusters. This should strictly follow the template of
#' \link{albufera_clusters}, and it is in fact unlikely that the user would want
#' to change this input (TODO #47).
#' @param management_df A `data.frame` that provides the yearly schedule for
#' irrigation and draining, that strictly follows the template of
#' \link{albufera_management}.
#' @param ideal_flow_rate_cm A positive number. Ideal inflow/outflow of a
#' cluster, for days in which the cluster is scheduled to be in flux (*i.e.*
#' when being simultaneously irrigated and drained) Expressed in centimeters
#' per day.
#' @param height_thresh_cm A positive number. Height threshold for water levels,
#' below which a cluster is considered to be emptied.
#'
#' @return An object of class \link{erahumed_simulation}.
#'
#' @details
#' TODO: #64, plus more detailed information on the structure of
#' `management_df` (and perhaps also on `clusters_df`, depending on the decision
#' taken regarding #47).
#'
#' @export
setup_hbp <- function(
    simulation,
    management_df = erahumed::albufera_management,
    clusters_df = erahumed::albufera_clusters,
    ideal_flow_rate_cm = 5,
    height_thresh_cm = 2
)
{
  setup_layer(simulation = simulation,
              layer = "hbp",
              management_df = management_df,
              clusters_df = clusters_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = height_thresh_cm,
              validate_params = validate_hbp_params
  )
}



compute_hbp_bare <- function(simulation)
{
  management_df <- get_layer_parameters(simulation, "hbp")[["management_df"]]
  clusters_df <- get_layer_parameters(simulation, "hbp")[["clusters_df"]]
  ideal_flow_rate_cm <- get_layer_parameters(simulation, "hbp")[["ideal_flow_rate_cm"]]
  height_thresh_cm <- get_layer_parameters(simulation, "hbp")[["height_thresh_cm"]]

  .hbp_args <- .hbp_data_prep(simulation = simulation,
                              management_df = management_df,
                              clusters_df = clusters_df,
                              ideal_flow_rate_cm = ideal_flow_rate_cm,
                              height_thresh_cm = height_thresh_cm)
  do.call(.hbp, .hbp_args)
}



validate_hbp_params <- function(management_df,
                                clusters_df,
                                ideal_flow_rate_cm,
                                height_thresh_cm)
{
  tryCatch(
    {
      assert_data.frame(management_df, template = erahumed::albufera_management)
      assert_data.frame(clusters_df, template = erahumed::albufera_clusters)
      assert_positive_number(ideal_flow_rate_cm)
      assert_positive_number(height_thresh_cm)
    },
    error = function(e) {
      class(e) <- c("validate_hbp_params_error", class(e))
      stop(e)
    }
  )
}



validate_hbp_output <- function(output) {
  assert_data.frame(output,
                    template =   data.frame(ideal_height_eod_cm = numeric(),
                                            height_eod_cm = numeric(),
                                            ideal_irrigation = logical(),
                                            ideal_draining = logical(),
                                            irrigation = logical(),
                                            draining = logical(),
                                            petp_cm = numeric(),
                                            area_m2 = numeric(),
                                            capacity_m3_s = numeric(),
                                            date = as.Date(character()),
                                            cluster_id = character(),
                                            ditch = character(),
                                            ideal_inflow_cm = numeric(),
                                            ideal_outflow_cm = numeric(),
                                            inflow_cm = numeric(),
                                            outflow_cm = numeric(),
                                            inflow_m3_s = numeric(),
                                            outflow_m3_s = numeric(),
                                            plan_delay = numeric()
                    )
  )

  assert_positive_vector(output$ideal_height_eod_cm, tol = 1e-6)
  assert_positive_vector(output$area_m2, tol = 1e-6)
  assert_positive_vector(output$capacity_m3_s, tol = 1e-6)
  assert_date(output$date)
  assert_character(output$cluster_id)
  assert_character(output$ditch)
  assert_positive_vector(output$ideal_outflow_cm, tol = 1e-6)
  assert_positive_vector(output$plan_delay, tol = 1e-6)
  assert_integer_vector(output$plan_delay)
  assert_positive_vector(output$height_eod_cm, tol = 1e-6)
}
