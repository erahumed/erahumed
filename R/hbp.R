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
compute_hbp <- function(
    model,
    management_df = erahumed::albufera_management,
    clusters_df = erahumed::albufera_clusters,
    ideal_flow_rate_cm = 5
    )
{
  compute_hbp_argcheck(model,
                       management_df,
                       clusters_df,
                       ideal_flow_rate_cm)

  .hbp_args <- hbp_data_prep(model = model,
                             management_df = management_df,
                             clusters_df = clusters_df,
                             ideal_flow_rate_cm = ideal_flow_rate_cm
                             )

  hbp_df <- do.call(.hbp, .hbp_args)
  hbp_obj <- make_hbp(hbp_df)

  model <- update_erahumed_model(model,
                                 outputs = list(hbp = hbp_obj),
                                 params = list(
                                   management_df = management_df,
                                   clusters_df = clusters_df,
                                   ideal_flow_rate_cm = ideal_flow_rate_cm
                                   )
                                 )
  return(model)
}

hbp_data_prep <- function(model,
                          management_df,
                          clusters_df,
                          ideal_flow_rate_cm)
{
  # res <- data.table::as.data.table( inp(model) ) |>
  #   merge(y = data.table::as.data.table( hba(model) ),
  #         by = "date",
  #         sort = FALSE,
  #         allow.cartesian = TRUE
  #         )

  res <- data.table::as.data.table(hba(model))  # Ugly but required atm

  res$petp_cm <- (res$rain_mm - res$evapotranspiration_mm) / 10
  res$mm <- get_mm(as.POSIXlt(res$date))
  res$dd <- get_dd(as.POSIXlt(res$date))

  res <- res |>
    merge(y = data.table::as.data.table(management_df),
          by = c("mm", "dd"),
          sort = FALSE,
          allow.cartesian = TRUE
          ) |>
    merge(y = data.table::as.data.table(clusters_df),
          by.x = c("tancat", "variety"),
          by.y = c("tancat", "rice_variety"),
          all.y = TRUE,
          sort = FALSE,
          allow.cartesian = TRUE
          )

  res <- data.table::setorderv(res, c("date", "cluster_id"))

  res <- list(date = res$date,
              ditch = res$ditch,
              cluster_id = res$cluster_id,
              ideal_height_cm = res$height_cm,
              petp_cm = res$petp_cm,
              seed_day = res$seed_day,
              ideal_irrigation = res$irrigation,
              ideal_draining = res$draining,
              area_m2 = res$area,
              total_inflow_lake = res$inflow_total,
              tancat = res$tancat,
              variety = res$variety,
              ideal_flow_rate_cm = ideal_flow_rate_cm
              )

  return(res)
}

compute_hbp_argcheck <- function(model, management_df, clusters_df, ideal_flow_rate_cm)
{
  tryCatch(
    {
      assert_erahumed_model(model)
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

hbp_precomputed <- function(formals, call) {
  # TODO: Reimplement similar mechanism for the final model
  return(NULL)

  file_path <- system.file("parquet",
                           "hbp.parquet",
                           package = "erahumed")
  if (!file.exists(file_path))
    return(NULL)

  if (! as.logical(Sys.getenv("erahumed_use_precomputed", "TRUE")) )
    return(NULL)

  call <- as.list(call)[-1]
  for (arg in names(formals)) {
    if (!arg %in% names(call))
      next
    provided_hash <- digest::digest(call[[arg]])
    default_hash <- digest::digest(formals[[arg]])
    if (provided_hash != default_hash)
      return(NULL)
  }

  return(arrow::read_parquet(file_path))
}

#' @rdname hbp
#' @export
hbp <- function(model) {
  assert_erahumed_model(model)
  return(model$output$hbp)
}
