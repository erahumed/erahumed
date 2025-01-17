#' @title `r erahumed_docs("layers", "hbp", "title")`
#' @name hbp
#'
#' @family simulation layers
#'
#' @description `r erahumed_docs("layers", "hbp", "description")`
#'
#' @inheritParams inp
#' @param management_df `r erahumed_param_roxy("management_df", "hbp")`
#' @param ideal_flow_rate_cm `r erahumed_param_roxy("ideal_flow_rate_cm", "hbp")`
#' @param height_thresh_cm `r erahumed_param_roxy("height_thresh_cm", "hbp")`
#'
#' @return An object of class \link{erahumed_simulation}.
#'
#' @details
#' The output `data.frame` for this model layer, retrieved through
#' `get_layer_output(layer = "hbp")` has one row per cluster and per day,
#' providing the simulated hydrological times-series for all paddy clusters. The
#' data-set has the following columns:
#'
#' \describe{
#'   \item{date}{Date of measurement}
#'   \item{cluster_id}{Cluster identifier.}
#'   \item{area_m2}{Surface area (in squared meters) of cluster.}
#'   \item{tancat}{`TRUE` or `FALSE`, whether the cluster is a *tancat*
#'   (*cf.* \link{info_clusters}).}
#'   \item{variety}{Rice variety cultivated in this cluster, simulated according
#'   to the `variety_prop` parameter of \link{setup_inp}.}
#'   \item{area_m2}{Surface area (in squared meters) of cluster.}
#'   \item{ditch}{Ditch to which the cluster pertains (*cf.*
#'   \link{info_clusters}).}
#'   \item{capacity_m3_s}{Outflow (to the Albufera Lake) of ditch.}
#'   \item{seed_day}{Whether `date` corresponds to the sowing day of the year.}
#'   \item{petp_cm}{Precipitation minus evapotranspiration per unit area, in
#'   centimeters (common to all clusters).}
#'   \item{ideal_draining}{`TRUE` or `FALSE`, whether the cluster is supposed to
#'   be drained on this day of the year.}
#'   \item{ideal_irrigation}{`TRUE` or `FALSE`, whether the cluster is supposed
#'   to be irrigated on this day of the year.}
#'   \item{ideal_inflow_cm}{Ideal inflow computed by the HBP algorithm
#'   (see TODO #46).}
#'   \item{ideal_outflow_cm}{Ideal outflow computed by the HBP algorithm
#'   (see TODO #46).}
#'   \item{ideal_diff_flow_cm}{Ideal inflow minus ideal outflow.}
#'   \item{ideal_height_eod_cm}{Ideal water level of cluster at the end of day
#'   (*i.e.* after draining and/or irrigation, for this day of the year.}
#'   \item{draining}{`TRUE` or `FALSE`, whether the cluster is actually (*i.e.*
#'   in the simulation) being drained on this day of the year.}
#'   \item{irrigation}{`TRUE` or `FALSE`, whether the cluster is actually (
#'   *i.e.* in the simulation) being irrigated on this day of the year.}
#'   \item{height_eod_cm}{Simulated water level of cluster at the end of day.}
#'   \item{height_sod_cm}{Simulated water level of cluster at the end of day (
#'   *i.e.* before irrigation and/or draining).}
#'   \item{inflow_cm}{Simulated inflow of cluster (in centimeters).}
#'   \item{inflow_m3_s}{Simulated inflow of cluster (in cube meters per
#'   second).}
#'   \item{outflow_cm}{Simulated outflow of cluster (in centimeters).}
#'   \item{outflow_m3_s}{Simulated outflow of cluster (in cube meters per
#'   second).}
#'   \item{plan_delay}{Accumulated delay on the yearly plan for this cluster.
#'   See (TODO #46) for a detailed description.}
#' }
#'
#' @noRd
setup_hbp <- function(
    simulation, management_df, ideal_flow_rate_cm, height_thresh_cm
    )
{
  tryCatch(
    {
      assert_erahumed_simulation(simulation)

      management_df_temp <- erahumed::albufera_management
      assert_data.frame(management_df, template = management_df_temp)
      dates <- paste(management_df$mm, management_df$dd)
      dates_expected <- paste(management_df_temp$mm, management_df_temp$dd)
      if (!setequal(unique(dates), unique(dates_expected))) {
        msg <- paste(
          "Provided 'management_df' has unexpected 'date' values.",
          "Values should coincide with 'erahumed::albufera_management$date'"
          )
        stop(msg)
      }

      assert_positive_number(ideal_flow_rate_cm)
      assert_positive_number(height_thresh_cm)

    },
    error = function(e) {
      class(e) <- c("validate_hbp_params_error", class(e))
      stop(e)
    }
  )

  setup_layer(layer = "hbp")
}



compute_hbp <- function(simulation)
{
  management_df <- get_layer_parameters(simulation, "hbp")[["management_df"]]
  clusters_df <- albufera_clusters
  ideal_flow_rate_cm <- get_layer_parameters(simulation, "hbp")[["ideal_flow_rate_cm"]]
  height_thresh_cm <- get_layer_parameters(simulation, "hbp")[["height_thresh_cm"]]
  cv_map <- get_layer_aux(simulation, "inp")[["cluster_variety_map"]]
  seed <- get_layer_parameters(simulation, "inp")[["seed"]]

  withr::with_seed(seed, {
    .hbp_args <- .hbp_data_prep(simulation = simulation,
                                management_df = management_df,
                                clusters_df = clusters_df,
                                ideal_flow_rate_cm = ideal_flow_rate_cm,
                                height_thresh_cm = height_thresh_cm,
                                cv_map = cv_map)

    output <- do.call(.hbp, .hbp_args)
    })

  validate_hbp_output(output)

  simulation [["hbp"]] [["output"]] <- output

  return(simulation)
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
