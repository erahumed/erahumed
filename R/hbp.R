#' @title Hydrological Balance of rice Paddies
#' @rdname hbp
#'
#' @description
#' Wrapper around \link{hbp}, used to run the local hydrological balance
#' simulation algorithm with the data for the Albufera lake packed up in the
#' `data.frame`s exported by `erahumed`.
#'
#' @param outflows_df,petp_df,management_df,clusters_df `data.frame`s, whose
#' structures follow the templates of
#' \link{albufera_outflows}, \link{albufera_petp}, \link{albufera_management}
#' and \link{albufera_clusters}, respectively.
#'
#' @return An object of class `hbp`, a lightweight wrapper of `data.frame`
#' with a few additional visualization methods (most prominently
#' \link{plot.hbp}).
#'
#' @details
#' The numeric inputs used from the linear storage curve and P-ETP surface were
#' extracted from the CHJ report
#' [Modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA.](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf)
#'
#' @export
hbp <- function(model)
  get_model_component(model, "hbp")

#' @rdname hbp
#' @export
compute_hbp <- function(
    model,
    management_df = erahumed::albufera_management,
    clusters_df = erahumed::albufera_clusters,
    ideal_flow_rate_cm = 5
    )
{
  compute_component(model,
                    "hbp",
                    management_df = management_df,
                    clusters_df = clusters_df,
                    ideal_flow_rate_cm = ideal_flow_rate_cm
                    )
}



compute_hbp_output <- function(
    model, management_df, clusters_df, ideal_flow_rate_cm
)
{
  .hbp_args <- .hbp_data_prep(model = model,
                              management_df = management_df,
                              clusters_df = clusters_df,
                              ideal_flow_rate_cm = ideal_flow_rate_cm
  )
  do.call(.hbp, .hbp_args)
}



compute_hbp_argcheck <- function(management_df, clusters_df, ideal_flow_rate_cm)
{
  tryCatch(
    {
      assert_data.frame(management_df, template = erahumed::albufera_management)
      assert_data.frame(clusters_df, template = erahumed::albufera_clusters)
      assert_positive_number(ideal_flow_rate_cm)
    },
    error = function(e) {
      class(e) <- c("compute_hbp_argcheck_error", class(e))
      stop(e)
    }
  )
}



compute_hbp_output <- function(
    model, management_df, clusters_df, ideal_flow_rate_cm
)
{
  .hbp_args <- .hbp_data_prep(model = model,
                              management_df = management_df,
                              clusters_df = clusters_df,
                              ideal_flow_rate_cm = ideal_flow_rate_cm
  )
  do.call(.hbp, .hbp_args)
}



hbp_validate_output <- function(output) {
  assert_data.frame(output,
                    template =   data.frame(ideal_height_cm = numeric(),
                                            real_height_cm = numeric(),
                                            ideal_irrigation = logical(),
                                            ideal_draining = logical(),
                                            real_irrigation = logical(),
                                            real_draining = logical(),
                                            petp_cm = numeric(),
                                            area_m2 = numeric(),
                                            capacity_m3_s = numeric(),
                                            date = as.Date(character()),
                                            cluster_id = character(),
                                            ditch = character(),
                                            ideal_inflow_cm = numeric(),
                                            ideal_outflow_cm = numeric(),
                                            real_inflow_cm = numeric(),
                                            real_outflow_cm = numeric(),
                                            real_inflow_m3_s = numeric(),
                                            real_outflow_m3_s = numeric(),
                                            plan_delay = numeric()
                    )
  )

  assert_positive_vector(output$ideal_height_cm, tol = 1e-6)
  assert_positive_vector(output$area_m2, tol = 1e-6)
  assert_positive_vector(output$capacity_m3_s, tol = 1e-6)
  assert_date(output$date)
  assert_character(output$cluster_id)
  assert_character(output$ditch)
  assert_positive_vector(output$ideal_outflow_cm, tol = 1e-6)
  assert_positive_vector(output$plan_delay, tol = 1e-6)
  assert_integer_vector(output$plan_delay)
  assert_positive_vector(output$real_height_cm, tol = 1e-6)
}


#' @export
print.erahumed_hbp <- function(x, ..., max = 100) {
  cat(bold("An object of class 'hbp'."))

  min_date <- format(as.Date(min(x$output$date)))
  max_date <- format(as.Date(max(x$output$date)))
  cat("\nData from:", min_date, "to:", max_date, "\n\n")

  print.data.frame(x$output, max = max)
}

#' @export
summary.erahumed_hbp <- function(object, ..., max = 100) {
  print(object, max = max)
}
